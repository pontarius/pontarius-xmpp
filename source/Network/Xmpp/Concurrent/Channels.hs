{-# OPTIONS_HADDOCK hide #-}
{-# LANGUAGE OverloadedStrings #-}
module Network.Xmpp.Concurrent.Channels
       ( module Network.Xmpp.Concurrent.Channels.Basic
       , module Network.Xmpp.Concurrent.Channels.Types
       , module Network.Xmpp.Concurrent.Channels.Message
       , module Network.Xmpp.Concurrent.Channels.Presence
       , module Network.Xmpp.Concurrent.Channels.IQ
       , toChans
       , newSession
       , writeWorker
       )

       where

import           Control.Applicative((<$>),(<*>))
import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Monad
import qualified Data.ByteString as BS
import           Data.IORef
import qualified Data.Map as Map
import           Data.Maybe (fromMaybe)
import           Data.XML.Types
import           Network.Xmpp.Concurrent.Channels.Basic
import           Network.Xmpp.Concurrent.Channels.IQ
import           Network.Xmpp.Concurrent.Channels.Message
import           Network.Xmpp.Concurrent.Channels.Presence
import           Network.Xmpp.Concurrent.Channels.Types
import           Network.Xmpp.Concurrent.Threads
import           Network.Xmpp.Concurrent.Types
import           Network.Xmpp.Marshal
import           Network.Xmpp.Pickle
import           Network.Xmpp.Types
import           Text.Xml.Stream.Elements

toChans :: TChan Stanza
        -> TVar IQHandlers
        -> Stanza
        -> IO ()
toChans stanzaC iqHands sta = atomically $ do
        writeTChan stanzaC sta
        case sta of
            IQRequestS     i -> handleIQRequest iqHands i
            IQResultS      i -> handleIQResponse iqHands (Right i)
            IQErrorS       i -> handleIQResponse iqHands (Left i)
            _                -> return ()
  where
    -- If the IQ request has a namespace, send it through the appropriate channel.
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


-- | Creates and initializes a new Xmpp context.
newSession :: Connection -> IO Session
newSession con = do
    outC <- newTChanIO
    stanzaChan <- newTChanIO
    iqHandlers <- newTVarIO (Map.empty, Map.empty)
    eh <- newTVarIO $ EventHandlers { connectionClosedHandler = \_ -> return () }
    let stanzaHandler = toChans stanzaChan iqHandlers
    (kill, wLock, conState, readerThread) <- startThreadsWith stanzaHandler eh con
    writer <- forkIO $ writeWorker outC wLock
    idRef <- newTVarIO 1
    let getId = atomically $ do
            curId <- readTVar idRef
            writeTVar idRef (curId + 1 :: Integer)
            return . read. show $ curId
    let cont = Context { writeRef = wLock
                       , readerThread = readerThread
                       , idGenerator = getId
                       , conRef = conState
                       , eventHandlers = eh
                       , stopThreads = kill >> killThread writer
                       }
    return $ Session { context = cont
                     , stanzaCh = stanzaChan
                     , outCh = outC
                     , iqHandlers = iqHandlers
                     }

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
