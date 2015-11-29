{-# LANGUAGE TupleSections #-}
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
  , newSession
  , session
  , newStanzaID
  , reconnect
  , reconnect'
  , reconnectNow
  , simpleAuth
  ) where

import           Control.Applicative ((<$>))
import           Control.Arrow (second)
import           Control.Concurrent (threadDelay)
import           Control.Concurrent.STM
import qualified Control.Exception as Ex
import           Control.Monad
import           Control.Monad.Error
import qualified Data.List as List
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
import           Network.Xmpp.IM.PresenceTracker
import           Network.Xmpp.IM.PresenceTracker.Types
import           Network.Xmpp.Sasl
import           Network.Xmpp.Sasl.Types
import           Network.Xmpp.Stream
import           Network.Xmpp.Tls
import           Network.Xmpp.Types
import           System.Log.Logger
import           System.Random (randomRIO)

import           Control.Monad.State.Strict


runHandlers :: [   XmppElement
                -> [Annotation]
                -> IO [Annotated XmppElement]
               ]
               -> XmppElement
               -> IO ()
runHandlers [] sta = do
    errorM "Pontarius.Xmpp" $
           "No stanza handlers set, discarding stanza" ++ show sta
    return ()
runHandlers hs sta = go hs sta []
  where go []        _  _   = return ()
        go (h:hands) sta' as = do
            res <- h sta' as
            forM_ res $ \(sta'', as') -> go hands sta'' (as ++ as')

toChan :: TChan (Annotated Stanza) -> StanzaHandler
toChan stanzaC _ sta as = do
    case sta of
     XmppStanza s -> atomically $ writeTChan stanzaC (s, as)
     _ -> return ()
    return [(sta, [])]

handleIQ :: TVar IQHandlers
         -> StanzaHandler
handleIQ _ _  s@XmppNonza{} _ = return [(s, [])]
handleIQ iqHands out s@(XmppStanza sta) as = do
        case sta of
            IQRequestS     i -> handleIQRequest iqHands i >> return []
            IQResultS      i -> handleIQResponse iqHands (Right i) >> return []
            IQErrorS       i -> handleIQResponse iqHands (Left i)  >> return []
            _                -> return [(s, [])]
  where
    -- If the IQ request has a namespace, send it through the appropriate channel.
    handleIQRequest :: TVar IQHandlers -> IQRequest -> IO ()
    handleIQRequest handlers iq = do
        res <- atomically $ do
            (byNS, _) <- readTVar handlers
            let iqNS = fromMaybe "" (nameNamespace . elementName
                                                 $ iqRequestPayload iq)
            case Map.lookup (iqRequestType iq, iqNS) byNS of
                Nothing -> return . Just $ serviceUnavailable iq
                Just ch -> do
                  sentRef <- newTMVar False
                  let answerT answer attrs = do
                          let IQRequest iqid from _to lang _tp bd _attrs = iq
                              response = case answer of
                                  Left er  -> IQErrorS $ IQError iqid Nothing
                                                                  from lang er
                                                                  (Just bd) attrs
                                  Right res -> IQResultS $ IQResult iqid Nothing
                                                                    from lang res
                                                                    attrs
                          Ex.bracketOnError (atomically $ takeTMVar sentRef)
                                            (atomically .  tryPutTMVar sentRef)
                                            $ \wasSent -> do
                              case wasSent of
                                  True -> do
                                      atomically $ putTMVar sentRef True
                                      return Nothing
                                  False -> do
                                      didSend <- out $ XmppStanza response
                                      case didSend of
                                          Right () -> do
                                              atomically $ putTMVar sentRef True
                                              return $ Just (Right ())
                                          er@Left{} -> do
                                              atomically $ putTMVar sentRef False
                                              return $ Just er
                  writeTChan ch $ IQRequestTicket answerT iq as
                  return Nothing
        maybe (return ()) (void . out . XmppStanza) res
    serviceUnavailable (IQRequest iqid from _to lang _tp bd _attrs) =
        IQErrorS $ IQError iqid Nothing from lang err (Just bd) []
    err = StanzaError Cancel ServiceUnavailable Nothing Nothing

    handleIQResponse :: TVar IQHandlers -> Either IQError IQResult -> IO ()
    handleIQResponse handlers iq = atomically $ do
        (byNS, byID) <- readTVar handlers
        case Map.updateLookupWithKey (\_ _ -> Nothing) (iqID iq) byID of
            (Nothing, _) -> return () -- The handler might be removed due to
                                      -- timeout
            (Just (expectedJid, tmvar), byID') -> do
                let expected = case expectedJid of
                    -- IQ was sent to the server and we didn't have a bound JID
                    -- We just accept any matching response
                        Left Nothing -> True
                    -- IQ was sent to the server and we had a bound JID. Valid
                    -- responses might have no to attribute, the domain of the
                    -- server, our bare JID or our full JID
                        Left (Just j) -> case iqFrom iq of
                            Nothing -> True
                            Just jf -> jf <~ j
                    -- IQ was sent to a (full) JID. The answer has to come from
                    -- the same exact JID.
                        Right j -> iqFrom iq == Just j
                case expected of
                    True -> do
                        let answer = Just (either IQResponseError
                                                  IQResponseResult iq, as)
                        _ <- tryPutTMVar tmvar answer -- Don't block.
                        writeTVar handlers (byNS, byID')
                    False -> return ()
      where
        iqID (Left err') = iqErrorID err'
        iqID (Right iq') = iqResultID iq'
        iqFrom (Left err') = iqErrorFrom err'
        iqFrom (Right iq') = iqResultFrom iq'

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
    ros <- case enableRoster config of
                False -> return $ Roster Nothing Map.empty
                True -> do
                    mbRos <- liftIO $ initialRoster config
                    return $ case mbRos of
                              Nothing -> Roster Nothing Map.empty
                              Just r -> r
    rosRef <- liftIO $ newTVarIO ros
    peers <- liftIO . newTVarIO $ Peers Map.empty
    rew <- lift $ newTVarIO 60
    let out = writeXmppElem writeSem
    boundJid <- liftIO $ withStream' (gets streamJid) stream
    let rosterH = if (enableRoster config)
                  then [handleRoster boundJid rosRef
                          (fromMaybe (\_ -> return ()) $ onRosterPush config)
                          (out)]
                  else []
    let presenceH = if (enablePresenceTracking config)
                    then [handlePresence (onPresenceChange config) peers out]
                    else []
    (sXmppElement, ps) <- initPlugins out $ plugins config
    let stanzaHandler = runHandlers $ List.concat
                        [ inHandler <$> ps
                        , [ toChan stanzaChan sXmppElement]
                        , presenceH
                        , rosterH
                        , [ handleIQ iqHands sXmppElement]
                        ]
    (kill, sState, reader) <- ErrorT $ startThreadsWith writeSem stanzaHandler
                                                        eh stream
                                                        (keepAlive config)
    idGen <- liftIO $ sessionStanzaIDs config
    let sess = Session { stanzaCh = stanzaChan
                       , iqHandlers = iqHands
                       , writeSemaphore = writeSem
                       , readerThread = reader
                       , idGenerator = idGen
                       , streamRef = sState
                       , eventHandlers = eh
                       , stopThreads = kill
                       , conf = config
                       , rosterRef = rosRef
                       , presenceRef = peers
                       , sendStanza' = sXmppElement . XmppStanza
                       , sRealm = realm
                       , sSaslCredentials = mbSasl
                       , reconnectWait = rew
                       }
    liftIO . atomically $ putTMVar eh $
        EventHandlers { connectionClosedHandler = onConnectionClosed config sess }
    -- Pass the new session to the plugins so they can "tie the knot"
    liftIO . forM_ ps $ \p -> onSessionUp p sess
    return sess
  where
    -- Pass the stanza out action to each plugin
    initPlugins out' = go out' []
      where
        go out ps' [] = return (out, ps')
        go out ps' (p:ps) = do
            p' <- p out
            go (outHandler p') (p' : ps') ps

connectStream :: HostName
              -> SessionConfiguration
              -> AuthData
              -> IO (Either XmppFailure Stream)
connectStream realm config mbSasl = do
    Ex.bracketOnError (openStream realm (sessionStreamConfiguration config))
                      (\s -> case s of
                                  Left _ -> return ()
                                  Right stream -> closeStreams stream)

        (\stream' -> case stream' of
              Left e -> return $ Left e
              Right stream -> do
                  res <- runErrorT $ do
                      ErrorT $ tls stream
                      cs <- liftIO $ withStream (gets streamConnectionState)
                                                stream
                      mbAuthError <- case mbSasl of
                          Nothing -> return Nothing
                          Just (handlers, resource) -> ErrorT $ auth (handlers cs)
                                                                resource stream
                      case mbAuthError of
                          Nothing -> return ()
                          Just e  -> throwError $ XmppAuthFailure e
                      return stream
                  case res of
                      Left e -> do
                          debugM "Pontarius.Xmpp" "Closing stream after error"
                          closeStreams stream
                          return (Left e)
                      Right r -> return $ Right r
                  )

-- | Creates a 'Session' object by setting up a connection with an XMPP server.
--
-- Will connect to the specified host with the provided configuration. If the
-- third parameter is a 'Just' value, @session@ will attempt to authenticate and
-- acquire an XMPP resource.
session :: HostName                          -- ^ The hostname / realm
        -> AuthData
        -> SessionConfiguration              -- ^ configuration details
        -> IO (Either XmppFailure Session)
session realm mbSasl config = runErrorT $ do
    stream <- ErrorT $ connectStream realm config mbSasl
    ses <- ErrorT $ newSession stream config realm mbSasl
    liftIO $ when (enableRoster config) $ initRoster ses
    return ses

-- | Authenticate using, in order of preference, 'scramSha1', 'digestMd5' and
-- finally, if both of those are not support and the stream is 'Secured' with
-- TLS, try 'plain'
--
-- The resource will be decided by the server
simpleAuth :: Username -> Password -> AuthData
simpleAuth uname pwd = Just (\cstate ->
                              [ scramSha1 uname Nothing pwd
                              , digestMd5 uname Nothing pwd
                              ] ++
                              if (cstate == Secured)
                              then [plain uname Nothing pwd]
                              else []
                            , Nothing)

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
        debugM "Pontarius.Xmpp" "reconnect: closing stream"
        closeStreams oldStream
        debugM "Pontarius.Xmpp" "reconnect: opening stream"
        s <- connectStream (sRealm sess) config (sSaslCredentials sess)
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
