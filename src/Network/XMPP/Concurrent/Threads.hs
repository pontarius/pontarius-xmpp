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
import Control.Monad.Trans
import Control.Monad.Reader
import Control.Monad.State.Strict

import qualified Data.ByteString as BS
import Data.Conduit
import qualified Data.Conduit.List as CL
import Data.Default (def)
import Data.IORef
import qualified Data.Map as Map
import Data.Maybe
import qualified Data.Text as Text

import Data.XML.Types

import Network.XMPP.Monad
import Network.XMPP.Marshal
import Network.XMPP.Pickle
import Network.XMPP.Concurrent.Types

import Text.XML.Stream.Elements
import qualified Text.XML.Stream.Render as XR

readWorker :: TChan (Either MessageError Message)
           -> TChan (Either PresenceError Presence)
           -> TVar IQHandlers
           -> TMVar XMPPConState
           -> IO ()
readWorker messageC presenceC handlers stateRef =
    Ex.mask_ . forever $ do
        s <- liftIO . atomically $ takeTMVar stateRef
        (sta', s') <- flip runStateT s $ Ex.catch ( do
                -- we don't know whether pull will necessarily be interruptible
                                           liftIO $ Ex.allowInterrupt
                                           Just <$> pull
                                         )
                                         (\(Interrupt t) -> do
                                           liftIO . atomically $
                                               putTMVar stateRef s
                                           liftIO . atomically $ takeTMVar t
                                           return Nothing
                                         )
        liftIO . atomically $ do
          case sta' of
              Nothing -> return ()
              Just sta -> do
                putTMVar stateRef s'
                case sta of
                    MessageS  m -> do writeTChan messageC $ Right m
                                      _ <- readTChan messageC -- Sic!
                                      return ()
                                   -- this may seem ridiculous, but to prevent
                                   -- the channel from filling up we immedtiately remove the
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


handleIQRequest handlers iq = do
  (byNS, _) <- readTVar handlers
  let iqNS = fromMaybe "" (nameNamespace . elementName $ iqRequestPayload iq)
  case Map.lookup (iqRequestType iq, iqNS) byNS of
      Nothing -> return () -- TODO: send error stanza
      Just ch -> do
        sent <- newTVar False
        writeTChan ch (iq, sent)

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
      iqID (Right iq) = iqResultID iq

writeWorker :: TChan Stanza -> TMVar (BS.ByteString -> IO ()) -> IO ()
writeWorker stCh writeR = forever $ do
  (write, next) <- atomically $ (,) <$>
                     takeTMVar writeR <*>
                     readTChan stCh
  _ <- write $ renderElement (pickleElem stanzaP next)
  atomically $ putTMVar writeR write

-- Two streams: input and output. Threads read from input stream and write to output stream.
-- | Runs thread in XmppState monad
-- returns channel of incoming and outgoing stances, respectively
-- and an Action to stop the Threads and close the connection
startThreads
  :: XMPPConMonad ( TChan (Either MessageError Message)
                  , TChan (Either PresenceError Presence)
                  , TVar IQHandlers
                  , TChan Stanza
                  , IO ()
                  , TMVar (BS.ByteString -> IO ())
                  , TMVar XMPPConState
                  , ThreadId
                  )

startThreads = do
  writeLock <- liftIO . newTMVarIO =<< gets sConPushBS
  messageC <- liftIO newTChanIO
  presenceC <- liftIO newTChanIO
  iqC  <- liftIO newTChanIO
  outC <- liftIO  newTChanIO
  handlers <- liftIO $ newTVarIO ( Map.empty, Map.empty)
  conS <- liftIO . newTMVarIO =<< get
  lw <- liftIO . forkIO $ writeWorker outC writeLock
  cp <- liftIO . forkIO $ connPersist writeLock
  s <- get
  rd <- liftIO . forkIO $ readWorker messageC presenceC handlers conS
  return (messageC, presenceC, handlers, outC
         , killConnection writeLock [lw, rd, cp]
         , writeLock, conS ,rd)
  where
      killConnection writeLock threads = liftIO $ do
        _ <- atomically $ takeTMVar writeLock -- Should we put it back?
        _ <- forM threads killThread
        return()

-- | Start worker threads and run action. The supplied action will run
-- in the calling thread. use 'forkXMPP' to start another thread.
runThreaded  :: XMPPThread a
                -> XMPPConMonad a
runThreaded a = do
    liftIO . putStrLn $ "starting threads"
    (mC, pC, hand, outC, _stopThreads, writeR, conS, rdr ) <- startThreads
    liftIO . putStrLn $ "threads running"
    workermCh <- liftIO . newIORef $ Nothing
    workerpCh <- liftIO . newIORef $ Nothing
    idRef <- liftIO $ newTVarIO 1
    let getId = atomically $ do
            curId <- readTVar idRef
            writeTVar idRef (curId + 1 :: Integer)
            return . read. show $ curId
    s <- get
    liftIO . putStrLn $ "starting application"
    liftIO $ runReaderT a (Thread workermCh workerpCh mC pC outC hand writeR rdr getId conS)


-- | Sends a blank space every 30 seconds to keep the connection alive
connPersist ::  TMVar (BS.ByteString -> IO ()) -> IO ()
connPersist lock = forever $ do
  pushBS <- atomically $ takeTMVar lock
  pushBS " "
  atomically $ putTMVar lock pushBS
--  putStrLn "<space added>"
  threadDelay 30000000
