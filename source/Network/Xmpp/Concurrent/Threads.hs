{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
module Network.Xmpp.Concurrent.Threads where

import Network.Xmpp.Types

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

import Network.Xmpp.Monad
import Network.Xmpp.Marshal
import Network.Xmpp.Pickle
import Network.Xmpp.Concurrent.Types

import Text.XML.Stream.Elements

import GHC.IO (unsafeUnmask)

-- Worker to read stanzas from the stream and concurrently distribute them to
-- all listener threads.
readWorker :: TChan (Either MessageError Message)
           -> TChan (Either PresenceError Presence)
           -> TChan Stanza
           -> TVar IQHandlers
           -> TVar EventHandlers
           -> TMVar XmppConnection
           -> IO ()
readWorker messageC presenceC stanzaC iqHands handlers stateRef =
    Ex.mask_ . forever $ do
        res <- liftIO $ Ex.catches ( do
                       -- we don't know whether pull will
                       -- necessarily be interruptible
                       s <- liftIO . atomically $ do
                            sr <- readTMVar stateRef
                            when (sConnectionState sr == XmppConnectionClosed)
                                 retry
                            return sr
                       allowInterrupt
                       Just . fst <$> runStateT pullStanza s
                       )
                   [ Ex.Handler $ \(Interrupt t) -> do
                         void $ handleInterrupts [t]
                         return Nothing
                   , Ex.Handler $ \(e :: StreamError) -> do
                         hands <- atomically $ readTVar handlers
                         _ <- forkIO $ connectionClosedHandler hands e
                         return Nothing
                   ]
        liftIO . atomically $ do
          case res of
              Nothing -> return ()
              Just sta -> do
                writeTChan stanzaC sta
                void $ readTChan stanzaC -- sic
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

                    IQRequestS     i -> handleIQRequest iqHands i
                    IQResultS      i -> handleIQResponse iqHands (Right i)
                    IQErrorS       i -> handleIQResponse iqHands (Left i)

  where
    -- Defining an Control.Exception.allowInterrupt equivalent for GHC 7
    -- compatibility.
    allowInterrupt :: IO ()
    allowInterrupt = unsafeUnmask $ return ()
    -- Call the connection closed handlers.
    noCon :: TVar EventHandlers -> StreamError -> IO (Maybe a)
    noCon h e = do
        hands <- atomically $ readTVar h
        _ <- forkIO $ connectionClosedHandler hands e
        return Nothing
    -- While waiting for the first semaphore(s) to flip we might receive another
    -- interrupt. When that happens we add it's semaphore to the list and retry
    -- waiting. We do this because we might receive another
    -- interrupt while we're waiting for a mutex to unlock; if that happens, the
    -- new interrupt is added to the list and is waited for as well.
    handleInterrupts :: [TMVar ()] -> IO [()]
    handleInterrupts ts =
        Ex.catch (atomically $ forM ts takeTMVar)
            (\(Interrupt t) -> handleInterrupts (t:ts))

-- If the IQ request has a namespace, sent it through the appropriate channel.
handleIQRequest :: TVar IQHandlers -> IQRequest -> STM ()
handleIQRequest handlers iq = do
  (byNS, _) <- readTVar handlers
  let iqNS = fromMaybe "" (nameNamespace . elementName $ iqRequestPayload iq)
  case Map.lookup (iqRequestType iq, iqNS) byNS of
      Nothing -> return () -- TODO: send error stanza
      Just ch -> do
        sent <- newTVar False
        writeTChan ch $ IQRequestTicket sent iq

handleIQResponse :: TVar IQHandlers -> Either IQError IQResult -> STM ()
handleIQResponse handlers iq = do
    (byNS, byID) <- readTVar handlers
    case Map.updateLookupWithKey (\_ _ -> Nothing) (iqID iq) byID of
        (Nothing, _) -> return () -- We are not supposed to send an error.
        (Just tmvar, byID') -> do
            let answer = either IQResponseError IQResponseResult iq
            _ <- tryPutTMVar tmvar answer -- Don't block.
            writeTVar handlers (byNS, byID')
  where
    iqID (Left err) = iqErrorID err
    iqID (Right iq') = iqResultID iq'

-- Worker to write stanzas to the stream concurrently.
writeWorker :: TChan Stanza -> TMVar (BS.ByteString -> IO Bool) -> IO ()
writeWorker stCh writeR = forever $ do
    (write, next) <- atomically $ (,) <$>
        takeTMVar writeR <*>
        readTChan stCh
    r <- write $ renderElement (pickleElem xpStanza next)
    atomically $ putTMVar writeR write
    unless r $ do
        atomically $ unGetTChan stCh next -- If the writing failed, the
                                          -- connection is dead.
        threadDelay 250000 -- Avoid free spinning.




-- Two streams: input and output. Threads read from input stream and write to
-- output stream.
-- | Runs thread in XmppState monad. Returns channel of incoming and outgoing
-- stances, respectively, and an Action to stop the Threads and close the
-- connection.
startThreads :: IO ( TChan (Either MessageError Message)
                   , TChan (Either PresenceError Presence)
                   , TChan Stanza
                   , TVar IQHandlers
                   , TChan Stanza
                   , IO ()
                   , TMVar (BS.ByteString -> IO Bool)
                   , TMVar XmppConnection
                   , ThreadId
                   , TVar EventHandlers
                   )
startThreads = do
    writeLock <- newTMVarIO (\_ -> return False)
    messageC <- newTChanIO
    presenceC <- newTChanIO
    outC <- newTChanIO
    stanzaC <- newTChanIO
    handlers <- newTVarIO (Map.empty, Map.empty)
    eh <- newTVarIO zeroEventHandlers
    conS <- newTMVarIO xmppNoConnection
    lw <- forkIO $ writeWorker outC writeLock
    cp <- forkIO $ connPersist writeLock
    rd <- forkIO $ readWorker messageC presenceC stanzaC handlers eh conS
    return ( messageC
           , presenceC
           , stanzaC
           , handlers
           , outC
           , killConnection writeLock [lw, rd, cp]
           , writeLock
           , conS
           , rd
           , eh)
  where
    killConnection writeLock threads = liftIO $ do
        _ <- atomically $ takeTMVar writeLock -- Should we put it back?
        _ <- forM threads killThread
        return ()
    zeroEventHandlers :: EventHandlers
    zeroEventHandlers = EventHandlers
        { connectionClosedHandler = \_ -> return ()
        }

-- | Creates and initializes a new concurrent session.
newSession :: IO Session
newSession = do
    (mC, pC, sC, hand, outC, stopThreads', writeR, conS, rdr, eh) <- startThreads
    workermCh <- newIORef $ Nothing
    workerpCh <- newIORef $ Nothing
    idRef <- newTVarIO 1
    let getId = atomically $ do
            curId <- readTVar idRef
            writeTVar idRef (curId + 1 :: Integer)
            return . read. show $ curId
    return $ Session
        mC
        pC
        sC
        workermCh
        workerpCh
        outC
        hand
        writeR
        rdr
        getId
        conS
        eh
        stopThreads'

-- | Creates a new session and runs the given Xmpp computation.
withNewSession :: Xmpp b -> IO (Session, b)
withNewSession a = do
    sess <- newSession
    ret <- runReaderT a sess
    return (sess, ret)

-- | Runs the given Xmpp computation in the given session.
withSession :: Session -> Xmpp a -> IO a
withSession = flip runReaderT

-- Acquires the write lock, pushes a space, and releases the lock.
-- | Sends a blank space every 30 seconds to keep the connection alive.
connPersist :: TMVar (BS.ByteString -> IO Bool) -> IO ()
connPersist lock = forever $ do
    pushBS <- atomically $ takeTMVar lock
    _ <- pushBS " "
    atomically $ putTMVar lock pushBS
    threadDelay 30000000 -- 30s
