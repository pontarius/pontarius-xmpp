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
  , StanzaHandler
  , newSession
  , writeWorker
  , session
  ) where

import           Control.Applicative((<$>),(<*>))
import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Monad
import           Control.Monad.Error
import qualified Data.ByteString as BS
import qualified Data.Map as Map
import           Data.Maybe
import           Data.Text as Text
import           Data.XML.Types
import           Network
import           Network.Xmpp.Concurrent.Basic
import           Network.Xmpp.Concurrent.IQ
import           Network.Xmpp.Concurrent.Message
import           Network.Xmpp.Concurrent.Monad
import           Network.Xmpp.Concurrent.Presence
import           Network.Xmpp.Concurrent.Threads
import           Network.Xmpp.Concurrent.Types
import           Network.Xmpp.IM.Roster.Types
import           Network.Xmpp.IM.Roster
import           Network.Xmpp.Marshal
import           Network.Xmpp.Sasl
import           Network.Xmpp.Sasl.Types
import           Network.Xmpp.Stream
import           Network.Xmpp.Tls
import           Network.Xmpp.Types
import           Network.Xmpp.Utilities

runHandlers :: (TChan Stanza) -> [StanzaHandler] -> Stanza -> IO ()
runHandlers _    []        _   = return ()
runHandlers outC (h:hands) sta = do
    res <- h outC sta
    case res of
        True -> runHandlers outC hands sta
        False -> return ()

toChan :: TChan Stanza -> StanzaHandler
toChan stanzaC _ sta = do
    atomically $ writeTChan stanzaC sta
    return True


handleIQ :: TVar IQHandlers
         -> StanzaHandler
handleIQ iqHands outC sta = atomically $ do
        case sta of
            IQRequestS     i -> handleIQRequest iqHands i >> return False
            IQResultS      i -> handleIQResponse iqHands (Right i) >> return False
            IQErrorS       i -> handleIQResponse iqHands (Left i) >> return False
            _                -> return True
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
        iqID (Left err') = iqErrorID err'
        iqID (Right iq') = iqResultID iq'

-- | Creates and initializes a new Xmpp context.
newSession :: Stream -> SessionConfiguration -> IO (Either XmppFailure Session)
newSession stream config = runErrorT $ do
    outC <- lift newTChanIO
    stanzaChan <- lift newTChanIO
    iqHands  <- lift $ newTVarIO (Map.empty, Map.empty)
    eh <- lift $ newTVarIO $ EventHandlers { connectionClosedHandler = sessionClosedHandler config }
    ros <- liftIO . newTVarIO $ Roster Nothing Map.empty
    let rosterH = if (enableRoster config) then handleRoster ros
                                           else \ _ _ -> return True
    let stanzaHandler = runHandlers outC $ Prelude.concat [ [ toChan stanzaChan ]
                                                          , extraStanzaHandlers
                                                                config
                                                          , [ handleIQ iqHands
                                                            , rosterH
                                                            ]
                                                          ]
    (kill, wLock, streamState, reader) <- ErrorT $ startThreadsWith stanzaHandler eh stream
    writer <- lift $ forkIO $ writeWorker outC wLock
    idGen <- liftIO $ sessionStanzaIDs config
    return $ Session { stanzaCh = stanzaChan
                     , outCh = outC
                     , iqHandlers = iqHands
                     , writeRef = wLock
                     , readerThread = reader
                     , idGenerator = idGen
                     , streamRef = streamState
                     , eventHandlers = eh
                     , stopThreads = kill >> killThread writer
                     , conf = config
                     , rosterRef = ros
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
-- Will connect to the specified host with the provided configuration. If the
-- third parameter is a 'Just' value, @session@ will attempt to authenticate and
-- acquire an XMPP resource.
session :: HostName                          -- ^ The hostname / realm
        -> Maybe ([SaslHandler], Maybe Text) -- ^ SASL handlers and the desired
                                             -- JID resource (or Nothing to let
                                             -- the server decide)
        -> SessionConfiguration              -- ^ configuration details
        -> IO (Either XmppFailure Session)
session realm mbSasl config = runErrorT $ do
    stream <- ErrorT $ openStream realm (sessionStreamConfiguration config)
    ErrorT $ tls stream
    mbAuthError <- case mbSasl of
        Nothing -> return Nothing
        Just (handlers, resource) -> ErrorT $ auth handlers resource stream
    case mbAuthError of
        Nothing -> return ()
        Just _  -> throwError XmppAuthFailure
    ses <- ErrorT $ newSession stream config
    liftIO $ when (enableRoster config) $ initRoster ses
    return ses
