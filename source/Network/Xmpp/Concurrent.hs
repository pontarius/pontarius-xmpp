{-# OPTIONS_HADDOCK hide #-}
{-# LANGUAGE OverloadedStrings #-}
module Network.Xmpp.Concurrent
  ( module Network.Xmpp.Concurrent.Monad
  , module Network.Xmpp.Concurrent.Threads
  , module Network.Xmpp.Concurrent.Basic
  , module Network.Xmpp.Concurrent.Types
  , module Network.Xmpp.Concurrent.Message
  , module Network.Xmpp.Concurrent.Presence
  , module Network.Xmpp.Concurrent.IQ
  , toChans
  , newSession
  , writeWorker
  , session
  ) where

import           Network.Xmpp.Concurrent.Monad
import           Network.Xmpp.Concurrent.Threads
import           Control.Applicative((<$>),(<*>))
import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Monad
import qualified Data.ByteString as BS
import           Data.IORef
import qualified Data.Map as Map
import           Data.Maybe (fromMaybe)
import           Data.XML.Types
import           Network.Xmpp.Concurrent.Basic
import           Network.Xmpp.Concurrent.IQ
import           Network.Xmpp.Concurrent.Message
import           Network.Xmpp.Concurrent.Presence
import           Network.Xmpp.Concurrent.Types
import           Network.Xmpp.Concurrent.Threads
import           Network.Xmpp.Marshal
import           Network.Xmpp.Types
import           Network
import           Data.Text as Text
import           Network.Xmpp.Tls
import qualified Network.TLS as TLS
import           Network.Xmpp.Sasl
import           Network.Xmpp.Sasl.Mechanisms
import           Network.Xmpp.Sasl.Types
import           Data.Maybe
import           Network.Xmpp.Stream
import           Network.Xmpp.Utilities

import           Control.Monad.Error
import Data.Default

toChans :: TChan Stanza
        -> TChan Stanza
        -> TVar IQHandlers
        -> Stanza
        -> IO ()
toChans stanzaC outC iqHands sta = atomically $ do
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
          Nothing -> writeTChan outC $ serviceUnavailable iq
          Just ch -> do
            sent <- newTVar False
            writeTChan ch $ IQRequestTicket sent iq
    serviceUnavailable (IQRequest iqid from _to lang _tp bd) =
        IQErrorS $ IQError iqid Nothing from lang err (Just bd)
    err = StanzaError Cancel ServiceUnavailable Nothing Nothing

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
newSession :: TMVar Stream -> SessionConfiguration -> IO (Either XmppFailure Session)
newSession stream config = runErrorT $ do
    outC <- lift newTChanIO
    stanzaChan <- lift newTChanIO
    iqHandlers <- lift $ newTVarIO (Map.empty, Map.empty)
    eh <- lift $ newTVarIO $ EventHandlers { connectionClosedHandler = \_ -> return () }
    let stanzaHandler = toChans stanzaChan outC iqHandlers
    (kill, wLock, streamState, readerThread) <- ErrorT $ startThreadsWith stanzaHandler eh stream
    writer <- lift $ forkIO $ writeWorker outC wLock
    idRef <- lift $ newTVarIO 1
    let getId = atomically $ do
            curId <- readTVar idRef
            writeTVar idRef (curId + 1 :: Integer)
            return . read. show $ curId
    return $ Session { stanzaCh = stanzaChan
                     , outCh = outC
                     , iqHandlers = iqHandlers
                     , writeRef = wLock
                     , readerThread = readerThread
                     , idGenerator = getId
                     , streamRef = streamState
                     , eventHandlers = eh
                     , stopThreads = kill >> killThread writer
                     , conf = config
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

-- | Creates a 'Session' object by setting up a connection with an XMPP server.
--
-- Will connect to the specified host. If the fourth parameters is a 'Just'
-- value, @session@ will attempt to secure the connection with TLS. If the fifth
-- parameters is a 'Just' value, @session@ will attempt to authenticate and
-- acquire an XMPP resource.
session :: HostName                          -- ^ The hostname / realm
        -> SessionConfiguration              -- ^ configuration details
        -> Maybe TLS.TLSParams               -- ^ TLS settings, if securing the
                                             -- connection to the server is
                                             -- desired
        -> Maybe ([SaslHandler], Maybe Text) -- ^ SASL handlers and the desired
                                             -- JID resource (or Nothing to let
                                             -- the server decide)
        -> IO (Either XmppFailure (Session, Maybe AuthFailure))
session realm config mbTls mbSasl = runErrorT $ do
    stream <- ErrorT $ openStream realm (sessionStreamConfiguration config)
    case mbTls of
        Nothing -> return ()
        Just tls -> ErrorT $ startTls tls stream
    aut <- case mbSasl of
        Nothing -> return Nothing
        Just (handlers, resource) -> ErrorT $ auth handlers resource stream
    ses <- ErrorT $ newSession stream config
    return (ses, aut)
