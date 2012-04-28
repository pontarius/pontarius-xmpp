{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
module Network.XMPP.Concurrent.Threads where

import Network.XMPP.Types

import Control.Applicative((<$>),(<*>))
import Control.Concurrent
import Control.Concurrent.STM
import qualified Control.Exception.Lifted as Ex
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Monad.State.Strict

import qualified Data.ByteString as BS
import Data.IORef
import qualified Data.Map as Map
import Data.Maybe

import Data.XML.Types

import Network.XMPP.Monad
import Network.XMPP.Marshal
import Network.XMPP.Pickle
import Network.XMPP.Concurrent.Types

import Text.XML.Stream.Elements

import GHC.IO (unsafeUnmask)

-- While waiting for the first semaphore(s) to flip we might receive
-- another interrupt. When that happens we add it's semaphore to the
-- list and retry waiting
handleInterrupts :: [TMVar ()] -> IO [()]
handleInterrupts ts =
    Ex.catch (atomically $ forM ts takeTMVar)
          ( \(Interrupt t) -> handleInterrupts (t:ts))

readWorker :: TChan (Either MessageError Message)
           -> TChan (Either PresenceError Presence)
           -> TVar IQHandlers
           -> TMVar XMPPConState
           -> IO ()
readWorker messageC presenceC handlers stateRef =
    Ex.mask_ . forever $ do
        res <- liftIO $ Ex.catch (
                   Ex.bracket
                       (atomically $ takeTMVar stateRef)
                       (atomically . putTMVar stateRef )
                       (\s -> do
                           -- we don't know whether pull will
                           -- necessarily be interruptible
                           allowInterrupt
                           Just <$> runStateT pullStanza s
                       )
                   )
                   (\(Interrupt t) -> do
                        void $ handleInterrupts [t]
                        return Nothing
                   )
        liftIO . atomically $ do
          case res of
              Nothing -> return ()
              Just (sta, s') -> do
                putTMVar stateRef s'
                case sta of
                    MessageS  m -> do writeTChan messageC $ Right m
                                      _ <- readTChan messageC -- Sic!
                                      return ()
                                   -- this may seem ridiculous, but to prevent
                                   -- the channel from filling up we
                                   -- immedtiately remove the
                                   -- Stanza we just put in. It will still be
                                   -- available in duplicates.
                    MessageErrorS m -> do writeTChan messageC $ Left m
                                          _ <- readTChan messageC
                                          return ()
                    PresenceS      p -> do
                                     writeTChan presenceC $ Right p
                                     _ <- readTChan presenceC
                                     return ()
                    PresenceErrorS p ->  do
                                           writeTChan presenceC $ Left p
                                           _ <- readTChan presenceC
                                           return ()

                    IQRequestS     i -> handleIQRequest handlers i
                    IQResultS      i -> handleIQResponse handlers (Right i)
                    IQErrorS       i -> handleIQResponse handlers (Left i)
  where
    -- Defining an Control.Exception.allowInterrupt equivalent for
    -- GHC 7 compatibility.
    allowInterrupt :: IO ()
    allowInterrupt = unsafeUnmask $ return ()

handleIQRequest :: TVar IQHandlers -> IQRequest -> STM ()
handleIQRequest handlers iq = do
  (byNS, _) <- readTVar handlers
  let iqNS = fromMaybe "" (nameNamespace . elementName $ iqRequestPayload iq)
  case Map.lookup (iqRequestType iq, iqNS) byNS of
      Nothing -> return () -- TODO: send error stanza
      Just ch -> do
        sent <- newTVar False
        writeTChan ch (iq, sent)

handleIQResponse :: TVar IQHandlers -> Either IQError IQResult -> STM ()
handleIQResponse handlers iq = do
  (byNS, byID) <- readTVar handlers
  case Map.updateLookupWithKey (\_ _ -> Nothing) (iqID iq) byID of
                       (Nothing, _) -> return () -- we are not supposed
                                                 -- to send an error
                       (Just tmvar, byID')  -> do
                           _ <- tryPutTMVar tmvar iq -- don't block
                           writeTVar handlers (byNS, byID')
    where
      iqID (Left err) = iqErrorID err
      iqID (Right iq') = iqResultID iq'

writeWorker :: TChan Stanza -> TMVar (BS.ByteString -> IO ()) -> IO ()
writeWorker stCh writeR = forever $ do
  (write, next) <- atomically $ (,) <$>
                     takeTMVar writeR <*>
                     readTChan stCh
  _ <- write $ renderElement (pickleElem xpStanza next)
  atomically $ putTMVar writeR write

-- Two streams: input and output. Threads read from input stream and write to output stream.
-- | Runs thread in XmppState monad
-- returns channel of incoming and outgoing stances, respectively
-- and an Action to stop the Threads and close the connection
startThreads
  :: IO ( TChan (Either MessageError Message)
        , TChan (Either PresenceError Presence)
        , TVar IQHandlers
        , TChan Stanza
        , IO ()
        , TMVar (BS.ByteString -> IO ())
        , TMVar XMPPConState
        , ThreadId
        , TVar EventHandlers
        )

startThreads = do
  writeLock <- newTMVarIO (\_ -> return ())
  messageC <- newTChanIO
  presenceC <- newTChanIO
  outC <- newTChanIO
  handlers <- newTVarIO ( Map.empty, Map.empty)
  eh <- newTVarIO  zeroEventHandlers
  conS <- newTMVarIO xmppZeroConState
  lw <- forkIO $ writeWorker outC writeLock
  cp <- forkIO $ connPersist writeLock
  rd <- forkIO $ readWorker messageC presenceC handlers conS
  return (messageC, presenceC, handlers, outC
         , killConnection writeLock [lw, rd, cp]
         , writeLock, conS ,rd, eh)
  where
      killConnection writeLock threads = liftIO $ do
        _ <- atomically $ takeTMVar writeLock -- Should we put it back?
        _ <- forM threads killThread
        return()

-- | Creates and initializes a new XMPP session.
newSession :: IO Session
newSession = do
    (mC, pC, hand, outC, stopThreads', writeR, conS, rdr, eh) <- startThreads
    workermCh <- newIORef $ Nothing
    workerpCh <- newIORef $ Nothing
    idRef <- newTVarIO 1
    let getId = atomically $ do
            curId <- readTVar idRef
            writeTVar idRef (curId + 1 :: Integer)
            return . read. show $ curId
    return (Session workermCh workerpCh mC pC outC hand writeR rdr getId conS eh stopThreads')

withNewSession :: XMPP b -> IO (Session, b)
withNewSession a = do
  sess <- newSession
  ret <- runReaderT a sess
  return (sess, ret)

withSession :: Session -> XMPP a -> IO a
withSession = flip runReaderT

-- | Sends a blank space every 30 seconds to keep the connection alive
connPersist ::  TMVar (BS.ByteString -> IO ()) -> IO ()
connPersist lock = forever $ do
  pushBS <- atomically $ takeTMVar lock
  pushBS " "
  atomically $ putTMVar lock pushBS
  threadDelay 30000000
