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
  , session
  , newStanzaID
  ) where

import           Control.Concurrent.STM
import           Control.Monad
import           Control.Monad.Error
import qualified Control.Exception as Ex
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
import           Network.Xmpp.Sasl
import           Network.Xmpp.Sasl.Types
import           Network.Xmpp.Stream
import           Network.Xmpp.Tls
import           Network.Xmpp.Types

import           Control.Monad.State.Strict

runHandlers :: WriteSemaphore -> [StanzaHandler] -> Stanza -> IO ()
runHandlers _    []        _   = return ()
runHandlers  sem (h:hands) sta = do
    res <- h sem sta
    case res of
        True -> runHandlers sem hands sta
        False -> return ()

toChan :: TChan Stanza -> StanzaHandler
toChan stanzaC _ sta = do
    atomically $ writeTChan stanzaC sta
    return True


handleIQ :: TVar IQHandlers
         -> StanzaHandler
handleIQ iqHands writeSem sta = do
        case sta of
            IQRequestS     i -> handleIQRequest iqHands i >> return False
            IQResultS      i -> handleIQResponse iqHands (Right i) >> return False
            IQErrorS       i -> handleIQResponse iqHands (Left i) >> return False
            _                -> return True
  where
    -- If the IQ request has a namespace, send it through the appropriate channel.
    handleIQRequest :: TVar IQHandlers -> IQRequest -> IO ()
    handleIQRequest handlers iq = do
        out <- atomically $ do
            (byNS, _) <- readTVar handlers
            let iqNS = fromMaybe "" (nameNamespace . elementName
                                                 $ iqRequestPayload iq)
            case Map.lookup (iqRequestType iq, iqNS) byNS of
                Nothing -> return . Just $ serviceUnavailable iq
                Just ch -> do
                  sentRef <- newTMVar False
                  let answerT answer = do
                          let IQRequest iqid from _to lang _tp bd = iq
                              response = case answer of
                                  Left er  -> IQErrorS $ IQError iqid Nothing
                                                                  from lang er
                                                                  (Just bd)
                                  Right res -> IQResultS $ IQResult iqid Nothing
                                                                    from lang res
                          Ex.bracketOnError (atomically $ takeTMVar sentRef)
                                            (atomically .  putTMVar sentRef)
                                            $ \wasSent -> do
                              case wasSent of
                                  True -> do
                                      atomically $ putTMVar sentRef True
                                      return Nothing
                                  False -> do
                                      didSend <- writeStanza writeSem response
                                      case didSend of
                                          True -> do
                                              atomically $ putTMVar sentRef True
                                              return $ Just True
                                          False -> do
                                              atomically $ putTMVar sentRef False
                                              return $ Just False
                  writeTChan ch $ IQRequestTicket answerT iq
                  return Nothing
        maybe (return ()) (void . writeStanza writeSem) out
    serviceUnavailable (IQRequest iqid from _to lang _tp bd) =
        IQErrorS $ IQError iqid Nothing from lang err (Just bd)
    err = StanzaError Cancel ServiceUnavailable Nothing Nothing

    handleIQResponse :: TVar IQHandlers -> Either IQError IQResult -> IO ()
    handleIQResponse handlers iq = atomically $ do
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
    write' <- liftIO $ withStream' (gets $ streamSend . streamHandle) stream
    writeSem <- liftIO $ newTMVarIO write'
    stanzaChan <- lift newTChanIO
    iqHands  <- lift $ newTVarIO (Map.empty, Map.empty)
    eh <- lift $ newTVarIO $ EventHandlers { connectionClosedHandler = onConnectionClosed config }
    ros <- liftIO . newTVarIO $ Roster Nothing Map.empty
    let rosterH = if (enableRoster config) then handleRoster ros
                                           else \ _ _ -> return True
    let stanzaHandler = runHandlers writeSem
                        $ Prelude.concat [ [ toChan stanzaChan ]
                                         , extraStanzaHandlers
                                           config
                                         , [ handleIQ iqHands
                                           , rosterH
                                           ]
                                         ]
    (kill, wLock, streamState, reader) <- ErrorT $ startThreadsWith writeSem stanzaHandler eh stream
    idGen <- liftIO $ sessionStanzaIDs config
    return $ Session { stanzaCh = stanzaChan
                     , iqHandlers = iqHands
                     , writeSemaphore = wLock
                     , readerThread = reader
                     , idGenerator = idGen
                     , streamRef = streamState
                     , eventHandlers = eh
                     , stopThreads = kill
                     , conf = config
                     , rosterRef = ros
                     }

-- | Creates a 'Session' object by setting up a connection with an XMPP server.
--
-- Will connect to the specified host with the provided configuration. If the
-- third parameter is a 'Just' value, @session@ will attempt to authenticate and
-- acquire an XMPP resource.
session :: HostName                          -- ^ The hostname / realm
        -> Maybe (ConnectionState -> [SaslHandler] , Maybe Text)
           -- ^ SASL handlers and the desired JID resource (or Nothing to let
           -- the server decide)
        -> SessionConfiguration              -- ^ configuration details
        -> IO (Either XmppFailure Session)
session realm mbSasl config = runErrorT $ do
    stream <- ErrorT $ openStream realm (sessionStreamConfiguration config)
    ErrorT $ tls stream
    cs <- liftIO $ withStream (gets streamConnectionState) stream
    mbAuthError <- case mbSasl of
        Nothing -> return Nothing
        Just (handlers, resource) -> ErrorT $ auth (handlers cs) resource stream
    case mbAuthError of
        Nothing -> return ()
        Just e  -> throwError $ XmppAuthFailure e
    ses <- ErrorT $ newSession stream config
    liftIO $ when (enableRoster config) $ initRoster ses
    return ses

newStanzaID :: Session -> IO StanzaID
newStanzaID = idGenerator
