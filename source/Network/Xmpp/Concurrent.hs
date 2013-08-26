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
  , reconnect
  , reconnect'
  , reconnectNow
  ) where

import           Control.Applicative ((<$>))
import           Control.Arrow (second)
import           Control.Concurrent (threadDelay)
import           Control.Concurrent.STM
import qualified Control.Exception as Ex
import           Control.Monad
import           Control.Monad.Error
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
import           Network.Xmpp.IM.Roster
import           Network.Xmpp.IM.Roster.Types
import           Network.Xmpp.Sasl
import           Network.Xmpp.Sasl.Types
import           Network.Xmpp.Stream
import           Network.Xmpp.Tls
import           Network.Xmpp.Types
import           System.Log.Logger
import           System.Random (randomRIO)

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
newSession :: Stream
           -> SessionConfiguration
           -> HostName
           -> Maybe (ConnectionState -> [SaslHandler] , Maybe Text)
           -> IO (Either XmppFailure Session)
newSession stream config realm mbSasl = runErrorT $ do
    write' <- liftIO $ withStream' (gets $ streamSend . streamHandle) stream
    writeSem <- liftIO $ newTMVarIO write'
    stanzaChan <- lift newTChanIO
    iqHands  <- lift $ newTVarIO (Map.empty, Map.empty)
    eh <- lift $ newEmptyTMVarIO
    ros <- liftIO . newTVarIO $ Roster Nothing Map.empty
    rew <- lift $ newTVarIO 60
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
    let sess = Session { stanzaCh = stanzaChan
                     , iqHandlers = iqHands
                     , writeSemaphore = wLock
                     , readerThread = reader
                     , idGenerator = idGen
                     , streamRef = streamState
                     , eventHandlers = eh
                     , stopThreads = kill
                     , conf = config
                     , rosterRef = ros
                     , sRealm = realm
                     , sSaslCredentials = mbSasl
                     , reconnectWait = rew
                     }
    liftIO . atomically $ putTMVar eh $ EventHandlers { connectionClosedHandler =
                                                 onConnectionClosed config sess }
    return sess

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
    ses <- ErrorT $ newSession stream config realm mbSasl
    liftIO $ when (enableRoster config) $ initRoster ses
    return ses

-- | Reconnect immediately with the stored settings. Returns @Just@ the error
-- when the reconnect attempt fails and Nothing when no failure was encountered.
--
-- This function does not set your presence to online, so you will have to do
-- this yourself.
reconnectNow :: Session -- ^ session to reconnect
          -> IO (Maybe XmppFailure)
reconnectNow sess@Session{conf = config, reconnectWait = rw} = do
    debugM "Pontarius.Xmpp" "reconnecting"
    res <- flip withConnection sess $ \oldStream -> do
        s <- runErrorT $ do
            liftIO $ debugM "Pontarius.Xmpp" "reconnect: closing stream"
            _ <- liftIO $ closeStreams oldStream
            liftIO $ debugM "Pontarius.Xmpp" "reconnect: opening stream"
            stream <- ErrorT $ openStream (sRealm sess)
                                          (sessionStreamConfiguration config)
            liftIO $ debugM "Pontarius.Xmpp" "reconnect: tls"
            ErrorT $ tls stream
            liftIO $ debugM "Pontarius.Xmpp" "reconnect: auth"
            cs <- liftIO $ withStream (gets streamConnectionState) stream
            mbAuthError <- case sSaslCredentials sess of
                Nothing -> return Nothing
                Just (handlers, resource) -> ErrorT $ auth (handlers cs)
                                                           resource stream
            case mbAuthError of
                Nothing -> return ()
                Just e  -> throwError $ XmppAuthFailure e
            return stream
        case s of
            Left  e -> do
                errorM "Pontarius.Xmpp" $ "reconnect failed"  ++ show e
                return (Left e   , oldStream )
            Right r -> return (Right () , r )
    case res of
        Left e -> return $ Just e
        Right (Left e) -> return $ Just e
        Right (Right ()) -> do
            atomically $ writeTVar rw 60
            when (enableRoster config) $ initRoster sess
            return Nothing


-- | Reconnect with the stored settings.
--
-- Waits a random amount of seconds (between 0 and 60 inclusive) before the
-- first attempt and an increasing amount after each attempt after that. Caps
-- out at 2-5 minutes.
--
-- This function does not set your presence to online, so you will have to do
-- this yourself.
reconnect :: Integer -- ^ Maximum number of retries (numbers of 1 or less will
                     -- perform exactly one retry)
          -> Session -- ^ Session to reconnect
          -> IO (Bool, [XmppFailure]) -- ^ Whether or not the reconnect attempt
                                      -- was successful, and a list of failure
                                      -- modes encountered
reconnect maxTries sess = go maxTries
  where
    go t = do
        res <- doRetry sess
        case res of
            Nothing -> return (True, [])
            Just e  -> if (t > 1) then (second (e:)) <$> go (t - 1)
                                  else return $ (False, [e])

-- | Reconnect with the stored settings with an unlimited number of retries.
--
-- Waits a random amount of seconds (between 0 and 60 inclusive) before the
-- first attempt and an increasing amount after each attempt after that. Caps
-- out at 2-5 minutes.
--
-- This function does not set your presence to online, so you will have to do
-- this yourself.
reconnect' :: Session -- ^ Session to reconnect
          -> IO Integer -- ^ Number of failed retries before connection could be
                        -- established
reconnect' sess = go 0
  where
    go i = do
        res <- doRetry sess
        case res of
            Nothing -> return i
            Just _e  -> go (i+1)

doRetry :: Session -> IO (Maybe XmppFailure)
doRetry sess@Session{reconnectWait = rw} = do
    wait <- atomically $ do
        wt <- readTVar rw
        writeTVar rw $ min 300 (2 * wt)
        return wt
    t <- randomRIO (wait `div` 2 - 30, max 60 wait)
    debugM "Pontarius.Xmpp" $
        "Waiting " ++ show t ++ " seconds before reconnecting"
    threadDelay $ t * 10^(6 :: Int)
    reconnectNow sess

-- | Generates a new stanza identifier based on the 'sessionStanzaIDs' field of
-- 'SessionConfiguration'.
newStanzaID :: Session -> IO Text
newStanzaID = idGenerator
