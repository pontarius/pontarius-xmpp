-- Copyright © 2010-2012 Jon Kristensen. See the LICENSE file in the
-- Pontarius distribution for more details.

-- I believe we need to use the MultiParamTypeClasses extension to be able to
-- work with arbitrary client states (solving the problem that the ClientState
-- type class is solving). However, I would be happy if someone proved me wrong.

{-# LANGUAGE MultiParamTypeClasses #-}

{-# OPTIONS_HADDOCK hide #-}

-- This module provides the functions used by XMPP clients to manage their XMPP
-- sessions.
--
-- Working with Pontarius is mostly done asynchronously with callbacks;
-- Pontarius "owns" the XMPP thread and carries the client state with it. A
-- client consists of a list of client handlers to handle XMPP events. This is
-- all set up through a @Session@ object, which a client can create by calling
-- the (blocking) function @createSession@.
--
-- The Pontarius XMPP functions operate in an arbitrary MonadIO monad.
-- Typically, clients will use the IO monad.
--
-- For more information, see the Pontarius manual.

-- TODO: Better functions and events for stanzas, IncomingIQ, OutgoingIQ, etc. (ClientSession, ClientStanza)

-- TODO: IO function to do everything related to the handle, instead of just connecting.

-- TODO: Enumerate in the same thread? Enumerate one element at the time, non-blocking?

module Network.XMPP.Session ( ClientHandler (..)
                            , ClientState (..)
                            , ConnectResult (..)
                            , Session
                            , TerminationReason
                            , OpenStreamResult (..)
                            , SecureWithTLSResult (..)
                            , AuthenticateResult (..)
                            , sendPresence
                            , sendIQ
                            , sendMessage
                            , connect
                            , openStreams
                            , tlsSecureStreams
                            , authenticate
                            , session
                            , injectAction
                            , getID ) where

import Network.XMPP.Address
import Network.XMPP.SASL
import Network.XMPP.Stanza
import Network.XMPP.Stream
import Network.XMPP.TLS
import Network.XMPP.Types
import Network.XMPP.Utilities

import qualified Control.Exception as CE
import qualified Control.Exception.Base as CEB -- ?
import qualified Control.Monad.Error as CME
import qualified Control.Monad.State as CMS
import qualified Network as N

-------------

import Crypto.Random (newGenIO, SystemRandom)

import Control.Concurrent.MVar

import Codec.Binary.UTF8.String
import Control.Concurrent.Chan (Chan, newChan, readChan, writeChan)
import Control.Concurrent (ThreadId, forkIO, killThread, threadDelay)
import Control.Monad.IO.Class (liftIO, MonadIO)
import Control.Monad.State hiding (State)
import Data.Enumerator (($$), Iteratee, continue, joinI,
                        run, run_, yield)
import Data.Enumerator.Binary (enumHandle, enumFile)
import Data.Maybe
import Data.String
import Data.XML.Types
import GHC.IO.Handle (Handle, hPutStr, hFlush, hSetBuffering, hWaitForInput)
import Network.TLS
import Network.TLS.Cipher
import System.IO (BufferMode, BufferMode(NoBuffering))
-- import Text.XML.Enumerator.Parse (parseBytes, decodeEntities)
-- import Text.XML.Enumerator.Document (fromEvents)
import qualified Codec.Binary.Base64.String as CBBS
import qualified Data.ByteString as DB
import qualified Data.ByteString.Lazy as DBL (ByteString, append, pack, fromChunks, toChunks, null)
import qualified Data.ByteString.Lazy.Char8 as DBLC (append, pack, unpack)
import qualified Data.Enumerator as E
import qualified Data.Enumerator.List as EL
import qualified Data.List as DL
import qualified Data.Text as DT
import qualified Data.Text.Lazy as DTL

import Data.Certificate.X509 (X509)

import Data.UUID (UUID, toString)

import System.Random (randomIO)



-- =============================================================================
--  EXPORTED TYPES AND FUNCTIONS
-- =============================================================================


-- | The @Session@ object is used by clients when interacting with Pontarius.
--   It holds information needed by Pontarius XMPP; its content is not
--   accessible from the client.

-- data Session s m = Session { sessionChannel :: Chan (InternalEvent s m)
--                            , sessionIDGenerator :: IDGenerator }


-- | A client typically needs one or more @ClientHandler@ objects to interact
--   with Pontarius. Each client handler may provide four callback
--   functions; the first three callbacks deals with received stanzas, and the
--   last one is used when the session is terminated.
--
--   These stanza functions takes the current client state and an object
--   containing the details of the stanza in question. The boolean returned
--   along with the possibly updated state signals whether or not the message
--   should be blocked to client handlerss further down the stack. For example,
--   an XEP-0030: Service Discovery handler may choose to hide disco\#info
--   requests to handlers above it in the stack.
--
--   The 'sessionTerminated' callback function takes a 'TerminationReason' value
--   along with the state and will be sent to all client handlers.

-- data MonadIO m => ClientHandler s m =
--   ClientHandler { messageReceived :: Maybe (Message -> StateT s m Bool)
--                 , presenceReceived :: Maybe (Presence -> StateT s m Bool)
--                 , iqReceived :: Maybe (IQ -> StateT s m Bool)
--                 , sessionTerminated :: Maybe (TerminationReason ->
--                                               StateT s m ()) }


-- | @TerminationReason@ contains information on why the XMPP session was
--   terminated.

-- data TerminationReason = WhateverReason -- TODO


-- | Creates an XMPP session. Blocks the current thread. The first parameter,
--   @s@, is an arbitrary state that is defined by the client. This is the
--   initial state, and it will be passed to the client (handlers) as XMPP
--   events are emitted. The second parameter is the list of @ClientHandler@s;
--   this is a way to provide a "layered" system of XMPP event handlers. For
--   example, a client may have a dedicated handler to manage messages,
--   implement a spam protection system, etc. Messages are piped through these
--   handlers one by one, and any handler may block the message from being sent
--   to the next handler(s) above in the stack. The third argument is a callback
--   function that will be called when the session has been initialized, and
--   this function should be used by the client to store the Session object in
--   its state.

-- Creates the internal event channel, injects the Pontarius XMPP session object
-- into the ClientState object, runs the "session created" client callback (in
-- the new state context), and stores the updated client state in s''. Finally,
-- we launch the (main) state loop of Pontarius XMPP.

session :: (MonadIO m, ClientState s m) => s -> [ClientHandler s m] ->
           (CMS.StateT s m ()) -> m ()

session s h c = do
  threadID <- liftIO $ newEmptyMVar
  chan <- liftIO $ newChan
  idGenerator <- liftIO $ idGenerator "" -- TODO: Prefix
  ((), clientState) <- runStateT c (putSession s $ session_ chan idGenerator)
  (result, _) <- runStateT (stateLoop chan)
                 (defaultState chan threadID h clientState idGenerator)
  case result of
    Just (CE.SomeException e) -> do
      liftIO $ putStrLn "Got an exception!"
      threadID' <- liftIO $ tryTakeMVar threadID
      case threadID' of
        Nothing -> do
          liftIO $ putStrLn "No thread ID to kill"
        Just t -> do
          liftIO $ putStrLn "Killing thread"
          liftIO $ killThread t
      CE.throw e
    Nothing ->
      return ()
  where
    -- session :: Chan (InternalEvent m s) -> Session m s -- TODO
    session_ c i = Session { sessionChannel = c, sessionIDGenerator = i }


defaultState :: (MonadIO m, ClientState s m) => Chan (InternalEvent s m) -> MVar ThreadId ->
                [ClientHandler s m] -> s -> IDGenerator -> State s m

defaultState c t h s i = State { stateClientHandlers = h
                               , stateClientState = s
                               , stateChannel = c
                               , stateConnectionState = Disconnected
                               , stateStreamState = PreStream
                               , stateTLSState = NoTLS
                               , stateOpenStreamsCallback = Nothing
                               , stateTLSSecureStreamsCallback = Nothing
                               , stateAuthenticateCallback = Nothing
                               , stateAuthenticationState = NoAuthentication
                               , stateResource = Nothing
                               , stateShouldExit = False
                               , stateThreadID = t
                               , statePresenceCallbacks = []
                               , stateMessageCallbacks = []
                               , stateIQCallbacks = []
                               , stateTimeoutStanzaIDs = []
                               , stateIDGenerator = i
                               , stateSASLRValue = Nothing } -- TODO: Prefix


-- |
-- Convenience function for calling "openStreams" and "tlsSecureStreams" and\/or
-- "authenticate". See the documentation for the three separate functions for
-- details on how they operate.

connect :: MonadIO m => Session s m -> HostName -> PortNumber ->
           Maybe (Maybe [X509], ([X509] -> Bool)) ->
           Maybe (UserName, Password, Maybe Resource) ->
           (ConnectResult -> StateT s m ()) -> StateT s m ()

connect s h p t a c = openStreams s h p connect'
  where
    connect' r = case r of
      OpenStreamSuccess _ _ -> case t of -- TODO: Check for TLS support?
        Just (certificate, certificateValidator) ->
          tlsSecureStreams s certificate certificateValidator connect''
        Nothing -> connect'' (SecureWithTLSSuccess 1.0 "") -- TODO
      OpenStreamFailure -> c ConnectOpenStreamFailure
    connect'' r = case r of
      SecureWithTLSSuccess _ _ -> case a of
        Just (userName, password, resource) ->
          authenticate s userName password resource connect'''
        Nothing -> connect''' (AuthenticateSuccess 1.0 "" "todo") -- TODO
      SecureWithTLSFailure -> c ConnectSecureWithTLSFailure
    connect''' r = case r of
      AuthenticateSuccess streamProperties streamFeatures resource ->
        c (ConnectSuccess streamProperties streamFeatures (Just resource))
      AuthenticateFailure -> c ConnectAuthenticateFailure


openStreams :: MonadIO m => Session s m -> HostName -> PortNumber ->
              (OpenStreamResult -> StateT s m ()) -> StateT s m ()

openStreams s h p c = CMS.get >>=
                     (\ state -> lift $ liftIO $ writeChan (sessionChannel s)
                                 (IEC (CEOpenStream h p c)))


-- |
-- Tries to secure the connection with TLS.
--
-- If the list of certificates is provided, they will be presented to the
-- server.
--
-- The third parameter is an optional custom validation function for the server
-- certificates. Note that Pontarius will perform its own validation
-- according to the RFC 6120, including comparing the domain name specified in
-- the certificate against the connected server, as well as checking the
-- integrity, and the certificate authorities.
--
-- Note: The current implementation of `certificate' looks for trusted
-- certificates in the /etc/ssl/certs directory.
--
-- Note: The current implementation of `certificate' does not support parsing
-- X509 extensions. Because of this, we will defer checking CRLs and/or OCSP
-- services as well as checking for the basicConstraints cA boolean for the
-- time-being.

tlsSecureStreams :: MonadIO m => Session s m -> Maybe [X509] ->
                 ([X509] -> Bool) -> (SecureWithTLSResult -> StateT s m ()) -> StateT s m ()

tlsSecureStreams s c a c_ = CMS.get >>=
                         (\ state -> lift $ liftIO $
                                     writeChan (sessionChannel s)
                                     (IEC (CESecureWithTLS c a c_)))


-- |

authenticate :: MonadIO m => Session s m -> UserName -> Password ->
                Maybe Resource -> (AuthenticateResult -> StateT s m ()) ->
                StateT s m ()

authenticate s u p r c = CMS.get >>=
                         (\ state -> lift $ liftIO $
                                     writeChan (sessionChannel s)
                                     (IEC (CEAuthenticate u p r c)))


sendMessage :: MonadIO m => Session s m -> Message -> Maybe (Message -> StateT s m Bool) -> Maybe (Timeout, StateT s m ()) -> Maybe (StreamError -> StateT s m ()) -> StateT s m ()
sendMessage se m c t st = CMS.get >>=
                  (\ state -> lift $ liftIO $
                              writeChan (sessionChannel se)
                              (IEC (CEMessage m c t st)))

sendPresence :: MonadIO m => Session s m -> Presence -> Maybe (Presence -> StateT s m Bool) -> Maybe (Timeout, StateT s m ()) -> Maybe (StreamError -> StateT s m ()) -> StateT s m ()
sendPresence se p c t st = CMS.get >>=
                   (\ state -> lift $ liftIO $
                               writeChan (sessionChannel se)
                               (IEC (CEPresence p c t st)))

sendIQ :: MonadIO m => Session s m -> IQ -> Maybe (IQ -> StateT s m Bool) -> Maybe (Timeout, StateT s m ()) -> Maybe (StreamError -> StateT s m ()) -> StateT s m ()
sendIQ se i c t st = CMS.get >>=
               (\ state -> lift $ liftIO $
                           writeChan (sessionChannel se)
                           (IEC (CEIQ i c t st)))

injectAction :: MonadIO m => Session s m -> Maybe (StateT s m Bool) -> StateT s m () -> StateT s m ()
injectAction s p a = CMS.get >>=
               (\ state -> lift $ liftIO $
                           writeChan (sessionChannel s)
                           (IEC (CEAction p a)))

getID :: MonadIO m => Session s m -> StateT s m String
getID s = CMS.get >>= \ state -> lift $ liftIO $ nextID (sessionIDGenerator s) >>= \ id -> return id

-- xmppDisconnect :: MonadIO m => Session s m -> Maybe (s -> (Bool, s)) -> m ()
-- xmppDisconnect s c = xmppDisconnect s c

class ClientState s m where
  putSession :: s -> Session s m -> s


-- =============================================================================
--  INTERNAL TYPES AND FUNCTIONS
-- =============================================================================


type OpenStreamCallback s m = Maybe (OpenStreamResult -> CMS.StateT s m ())

type SecureWithTLSCallback s m = Maybe (SecureWithTLSResult -> CMS.StateT s m ())

type AuthenticateCallback s m = Maybe (AuthenticateResult -> CMS.StateT s m ())


isConnected :: ConnectionState -> Bool
isConnected Disconnected = True
isConnected (Connected _ _) = True

data MonadIO m => State s m =
  State { stateClientHandlers :: [ClientHandler s m]
        , stateClientState :: s
        , stateChannel :: Chan (InternalEvent s m)
        , stateConnectionState :: ConnectionState -- s m
        , stateTLSState :: TLSState
        , stateStreamState :: StreamState
        , stateOpenStreamsCallback :: OpenStreamCallback s m
        , stateTLSSecureStreamsCallback :: SecureWithTLSCallback s m
        , stateAuthenticateCallback :: AuthenticateCallback s m
        , stateAuthenticationState :: AuthenticationState
        , stateResource :: Maybe Resource
        , stateShouldExit :: Bool
        , stateThreadID :: MVar ThreadId
        , statePresenceCallbacks :: [(StanzaID, (Presence -> StateT s m Bool))]
        , stateMessageCallbacks :: [(StanzaID, (Message -> StateT s m Bool))]
        , stateIQCallbacks :: [(StanzaID, (IQ -> StateT s m Bool))]
        , stateTimeoutStanzaIDs :: [StanzaID]
        , stateIDGenerator :: IDGenerator
        , stateSASLRValue :: Maybe String
        }


-- Repeatedly reads internal events from the channel and processes them. This is
-- the main loop of the XMPP session process.

-- The main loop of the XMPP library runs in the following monads:
--
-- m, m => MonadIO (from the client)
-- StateT
-- ErrorT

-- TODO: Will >> carry the updated state?
-- TODO: Should InternalState be in both places?

stateLoop :: (MonadIO m, ClientState s m) => Chan (InternalEvent s m) ->
             StateT (State s m) m (Maybe CE.SomeException)

stateLoop c = do
  event <- lift $ liftIO $ readChan c
  lift $ liftIO $ putStrLn $ "Processing event " ++ (show event) ++ "."
  result <- (processEvent event)
  state <- get
  case result of
    Nothing -> do
      case stateShouldExit state of
        True ->
          return $ Nothing
        False ->
          stateLoop c
    Just e ->
      return $ Just e


-- Process an InternalEvent and performs the necessary IO and updates the state
-- accordingly.

processEvent :: (MonadIO m, ClientState s m) => (InternalEvent s m) ->
                (StateT (State s m) m) (Maybe CE.SomeException)

processEvent e = get >>= \ state ->
  let handleOrTLSCtx = case stateTLSState state of
        PostHandshake tlsCtx ->
          Right tlsCtx
        _ ->
          let Connected _ handle = stateConnectionState state in Left handle
  in case e of

  -- ---------------------------------------------------------------------------
  --  CLIENT EVENTS
  -- ---------------------------------------------------------------------------
  --
  IEC (CEOpenStream hostName portNumber callback) -> do

    -- CEB.assert (stateConnectionState state == Disconnected) (return ())

    -- let portNumber' = fromIntegral portNumber

    -- connectResult <- liftIO $ CE.try $ N.connectTo hostName
    --                  (N.PortNumber portNumber')

    -- case connectResult of
      -- Right handle -> do
        -- put $ state { stateConnectionState = Connected (ServerAddress hostName portNumber') handle
        --             , stateStreamState = PreStream
        --             , stateOpenStreamsCallback = Just callback }
        -- lift $ liftIO $ hSetBuffering handle NoBuffering
        -- lift $ liftIO $ send ("<?xml version='1.0'?><stream:stream to='" ++ hostName ++
        --   "' xmlns='jabber:client' xmlns:stream='http://etherx.jabber.or" ++
        --   "g/streams' version='1.0'>") (Left handle)
        -- threadID <- lift $ liftIO $ forkIO $ xmlEnumerator (stateChannel state) (Left handle)
        -- lift $ liftIO $ putMVar (stateThreadID state) threadID
        -- return Nothing
      Left e -> do
        let clientState = stateClientState state
        ((), clientState') <- lift $ runStateT (callback OpenStreamFailure) clientState
        put $ state { stateShouldExit = True }
        return $ Just e

  IEC (CESecureWithTLS certificate verifyCertificate callback) -> do
    -- CEB.assert (not $ isTLSSecured (stateStreamState state)) (return ())
    let Connected _ handle = stateConnectionState state
    lift $ liftIO $ send "<starttls xmlns='urn:ietf:params:xml:ns:xmpp-tls'/>" (Left handle)
    put $ state { stateStreamState = PreStream
                , stateTLSSecureStreamsCallback = Just callback }
    return Nothing

-- TODO: Save callback in state.
  IEC (CEAuthenticate userName password resource callback) -> do
    -- CEB.assert (or [ stateConnectionState state == Connected
    --                , stateConnectionState state == TLSSecured ]) (return ())
    -- CEB.assert (stateHandle state /= Nothing) (return ())
    -- let Connected (ServerAddress hostName _) _ = stateConnectionState state
    rValue <- lift $ liftIO $ randomIO
    put $ state { stateAuthenticationState = AuthenticatingPreChallenge1 userName password resource
                , stateAuthenticateCallback = Just callback
                , stateSASLRValue = Just (toString rValue) }
    lift $ liftIO $ putStrLn $ "__________" ++ ("<auth xmlns='urn:ietf:params:xml:ns:xmpp-sasl' mechanism='SCRAM-SHA-1'>" ++ (CBBS.encode ("n,,n=" ++ userName ++ ",r=" ++ (toString rValue))) ++ "</auth>")
    lift $ liftIO $ send ("<auth xmlns='urn:ietf:params:xml:ns:xmpp-sasl' mechanism='SCRAM-SHA-1'>" ++ (CBBS.encode ("n,,n=" ++ userName ++ ",r=" ++ (toString rValue))) ++ "</auth>") handleOrTLSCtx
    return Nothing

  IEE (EnumeratorBeginStream from to id ver lang namespace) -> do
    put $ state { stateStreamState = PreFeatures (1.0) }
    return Nothing

--  IEE (EnumeratorXML (XEFeatures features)) -> do
--    let PreFeatures streamProperties = stateStreamState state
--    case stateTLSState state of
--      NoTLS -> let callback = fromJust $ stateOpenStreamsCallback state in do
--        ((), clientState) <- lift $ runStateT (callback $ OpenStreamSuccess streamProperties "TODO") (stateClientState state)
--        put $ state { stateClientState = clientState
--                    , stateStreamState = PostFeatures streamProperties "TODO" }
--        return Nothing
--      _ -> case stateAuthenticationState state of
--        AuthenticatedUnbound _ resource -> do -- TODO: resource
--          case resource of
--            Nothing -> do
--              lift $ liftIO $ send ("<iq type=\"set\" id=\"bind_1\"><bind xmlns=\"urn:ietf:param" ++ "s:xml:ns:xmpp-bind\"></bind></iq>") handleOrTLSCtx
--              return ()
--            _ -> do
--              lift $ liftIO $ send ("<iq type=\"set\" id=\"bind_1\"><bind xmlns=\"urn:ietf:param" ++ "s:xml:ns:xmpp-bind\"><resource>" ++ fromJust resource ++ "</resource></bind></iq>") handleOrTLSCtx
--              return ()
--          id <- liftIO $ nextID $ stateIDGenerator state
--          lift $ liftIO $ send ("<iq type=\"set\" id=\"" ++ id ++ "\"><session xmlns=\"urn:ietf:params:xml:ns:xmpp-session\"/>" ++ "</iq>") handleOrTLSCtx
--
--          -- TODO: Execute callback on iq result
--
--          let callback = fromJust $ stateAuthenticateCallback state in do -- TODO: streamProperties "TODO" after success
--            ((), clientState) <- lift $ runStateT (callback $ AuthenticateSuccess streamProperties "TODO" "todo") (stateClientState state) -- get proper resource value when moving to iq result
--            put $ state { stateClientState = clientState
--                        , stateStreamState = PostFeatures streamProperties "TODO" }
--          state' <- get
--          return Nothing
--        _ -> do
--          let callback = fromJust $ stateTLSSecureStreamsCallback state in do
--          ((), clientState) <- lift $ runStateT (callback $ SecureWithTLSSuccess streamProperties "TODO") (stateClientState state)
--          put $ state { stateClientState = clientState
--                      , stateStreamState = PostFeatures streamProperties "TODO" }
--          return Nothing
--
--  -- TODO: Can we assume that it's safe to start to enumerate on handle when it
--  -- might not have exited?
--  IEE (EnumeratorXML XEProceed) -> do
--    let Connected (ServerAddress hostName _) handle = stateConnectionState state
--    tlsCtx <- lift $ liftIO $ do
--        gen <- newGenIO :: IO SystemRandom -- TODO: Investigate limitations
--        clientContext <- client tlsParams gen handle
--        handshake clientContext
--        return clientContext
--    put $ (defaultState (stateChannel state) (stateThreadID state) (stateClientHandlers state) (stateClientState state) (stateIDGenerator state)) { stateTLSState = PostHandshake tlsCtx, stateConnectionState = (stateConnectionState state), stateTLSSecureStreamsCallback = (stateTLSSecureStreamsCallback state) }
--    threadID <- lift $ liftIO $ forkIO $ xmlEnumerator (stateChannel state) (Right tlsCtx) -- double code
--    lift $ liftIO $ putStrLn "00000000000000000000000000000000"
--    lift $ liftIO $ swapMVar (stateThreadID state) threadID -- return value not used
--    lift $ liftIO $ putStrLn "00000000000000000000000000000000"
--    lift $ liftIO $ threadDelay 1000000
--    lift $ liftIO $ putStrLn "00000000000000000000000000000000"
--    lift $ liftIO $ send ("<?xml version='1.0'?><stream:stream to='" ++
--      hostName ++ "' xmlns='jabber:client' xmlns:stream='http://etherx.jabber.org/" ++
--      "streams' version='1.0'>") (Right tlsCtx)
--    lift $ liftIO $ putStrLn "00000000000000000000000000000000"
--    return Nothing
--
--  IEE (EnumeratorXML (XEChallenge (Chal challenge))) -> do
--    lift $ liftIO $ putStrLn challenge
--    let Connected (ServerAddress hostName _) _ = stateConnectionState state
--    let challenge' = CBBS.decode challenge
--    case stateAuthenticationState state of
--      AuthenticatingPreChallenge1 userName password resource -> do
--        id <- liftIO $ nextID $ stateIDGenerator state
--        -- TODO: replyToChallenge
--        return ()
--      AuthenticatingPreChallenge2 userName password resource -> do
--        -- This is not the first challenge; [...]
--        -- TODO: Can we assume "rspauth"?
--        lift $ liftIO $ send "<response xmlns='urn:ietf:params:xml:ns:xmpp-sasl'/>" handleOrTLSCtx
--        put $ state { stateAuthenticationState = AuthenticatingPreSuccess userName password resource }
--        return ()
--    return Nothing
--
--  -- We have received a SASL "success" message over a secured connection
--  -- TODO: Parse the success message?
--  -- TODO: <?xml version='1.0'?>?
--  IEE (EnumeratorXML (XESuccess (Succ _))) -> do
--    let serverHost = "jonkristensen.com"
--    let AuthenticatingPreSuccess userName _ resource = stateAuthenticationState state in do
--      lift $ liftIO $ send ("<?xml version='1.0'?><stream:stream to='" ++ serverHost ++ "' xmlns='jabber:client' xmlns:stream='http://etherx.jabber.org/" ++ "streams' version='1.0'>") handleOrTLSCtx
--      put $ state { stateAuthenticationState = AuthenticatedUnbound userName resource }
--    return Nothing

  IEE EnumeratorDone ->
    -- TODO: Exit?
    return Nothing

  -- ---------------------------------------------------------------------------
  --  XML EVENTS
  -- ---------------------------------------------------------------------------

--  -- Ignore id="bind_1" and session IQ result, otherwise create client event
--  IEE (EnumeratorXML (XEIQ iqEvent)) ->
--    case shouldIgnoreIQ iqEvent of
--        True ->
--            return Nothing
--        False -> do
--            let stanzaID' = iqID iqEvent
--            let newTimeouts = case stanzaID' of
--                                Just stanzaID'' ->
--                                    case stanzaID'' `elem` (stateTimeoutStanzaIDs state) of
--                                                True -> filter (\ e -> e /= stanzaID'') (stateTimeoutStanzaIDs state)
--                                                False -> (stateTimeoutStanzaIDs state)
--                                Nothing -> (stateTimeoutStanzaIDs state)
--            let iqReceivedFunctions = map (\ x -> iqReceived x) (stateClientHandlers state)
--            let functions = map (\ x -> case x of
--                                    Just f -> Just (f iqEvent)
--                                    Nothing -> Nothing) iqReceivedFunctions
--            let functions' = case lookup (fromJust $ iqID $ iqEvent) (stateIQCallbacks state) of
--                                  Just f -> (Just (f $ iqEvent)):functions
--                                  Nothing -> functions
--            let clientState = stateClientState state
--            clientState' <- sendToClient functions' clientState
--            put $ state { stateClientState = clientState', stateTimeoutStanzaIDs = newTimeouts }
--            return Nothing
--
--  -- TODO: Known bug - does not work with PresenceError
--
--  IEE (EnumeratorXML (XEPresence (Right presenceEvent))) -> do
--    let stanzaID' = presenceID $ presenceEvent
--    let newTimeouts = case stanzaID' of
--                        Just stanzaID'' ->
--                            case stanzaID'' `elem` (stateTimeoutStanzaIDs state) of
--                                    True -> filter (\ e -> e /= stanzaID'') (stateTimeoutStanzaIDs state)
--                                    False -> (stateTimeoutStanzaIDs state)
--                        Nothing -> (stateTimeoutStanzaIDs state)
--    let presenceReceivedFunctions = map (\ x -> presenceReceived x) (stateClientHandlers state)
--    let functions = map (\ x -> case x of
--                            Just f -> Just (f presenceEvent)
--                            Nothing -> Nothing) presenceReceivedFunctions
--    let clientState = stateClientState state -- ClientState s m
--    clientState' <- sendToClient functions clientState
--    put $ state { stateClientState = clientState', stateTimeoutStanzaIDs = newTimeouts }
--    return Nothing
--
--  -- TODO: Does not work with message errors
--  IEE (EnumeratorXML (XEMessage (Right messageEvent))) -> do
--    let stanzaID' = messageID $ messageEvent
--    let newTimeouts = case stanzaID' of
--                        Just stanzaID'' ->
--                            case stanzaID'' `elem` (stateTimeoutStanzaIDs state) of
--                                    True -> filter (\ e -> e /= stanzaID'') (stateTimeoutStanzaIDs state)
--                                    False -> (stateTimeoutStanzaIDs state)
--                        Nothing -> (stateTimeoutStanzaIDs state)
--    let messageReceivedFunctions = map (\ x -> messageReceived x) (stateClientHandlers state)
--    let functions = map (\ x -> case x of
--                            Just f -> Just (f messageEvent)
--                            Nothing -> Nothing) messageReceivedFunctions
--    let clientState = stateClientState state -- ClientState s m
--    clientState' <- sendToClient functions clientState
--    put $ state { stateClientState = clientState', stateTimeoutStanzaIDs = newTimeouts }
--    return Nothing

  IEC (CEPresence presence stanzaCallback timeoutCallback streamErrorCallback) -> do
    presence' <- case presenceID $ presence of
      Nothing -> do
        id <- liftIO $ nextID $ stateIDGenerator state
        return $ presence { presenceID = Just (SID id) }
      _ -> return presence
    case timeoutCallback of
        Just (t, timeoutCallback') ->
            let stanzaID' = (fromJust $ presenceID $ presence') in do
                registerTimeout (stateChannel state) stanzaID' t timeoutCallback'
                put $ state { stateTimeoutStanzaIDs = stanzaID':(stateTimeoutStanzaIDs state) }
        Nothing ->
            return ()
    let xml = presenceToXML (Right presence') (fromJust $ langTag "en")
    lift $ liftIO $ send (elementToString $ Just xml) handleOrTLSCtx
    return Nothing

  IEC (CEMessage message stanzaCallback timeoutCallback streamErrorCallback) -> do
    message' <- case messageID message of
      Nothing -> do
        id <- liftIO $ nextID $ stateIDGenerator state
        return $ message { messageID = Just (SID id) }
      _ -> return message
    case timeoutCallback of
        Just (t, timeoutCallback') ->
            let stanzaID' = (fromJust $ messageID message') in do
                registerTimeout (stateChannel state) stanzaID' t timeoutCallback'
                put $ state { stateTimeoutStanzaIDs = stanzaID':(stateTimeoutStanzaIDs state) }
        Nothing ->
            return ()
    let xml = messageToXML (Right message') (fromJust $ langTag "en")
    lift $ liftIO $ send (elementToString $ Just xml) handleOrTLSCtx
    return Nothing

  -- TODO: Known bugs until Session rewritten - new ID everytime, callback not called

  IEC (CEIQ iq stanzaCallback timeoutCallback stanzaErrorCallback) -> do
    iq' <- do -- case iqID iq of
      -- Nothing -> do
        id <- liftIO $ nextID $ stateIDGenerator state
        return iq
    let callback' = fromJust stanzaCallback
    put $ state { stateIQCallbacks = (fromJust $ iqID iq, callback'):(stateIQCallbacks state) }
    case timeoutCallback of
        Just (t, timeoutCallback') ->
            let stanzaID' = (fromJust $ iqID iq') in do
                registerTimeout (stateChannel state) stanzaID' t timeoutCallback'
                put $ state { stateTimeoutStanzaIDs = stanzaID':(stateTimeoutStanzaIDs state) }
        Nothing ->
            return ()
    -- TODO: Bind ID to callback
    let xml = iqToXML iq' (fromJust $ langTag "en")
    lift $ liftIO $ send (elementToString $ Just xml) handleOrTLSCtx
    return Nothing

  IEC (CEAction predicate callback) -> do
    case predicate of
        Just predicate' -> do
            result <- runBoolClientCallback predicate'
            case result of
                True -> do
                    runUnitClientCallback callback
                    return Nothing
                False -> return Nothing
        Nothing -> do
            runUnitClientCallback callback
            return Nothing

  -- XOEDisconnect -> do
  --   -- TODO: Close stream
  --   return ()

  IET (TimeoutEvent i t c) ->
    case i `elem` (stateTimeoutStanzaIDs state) of
        True -> do
            runUnitClientCallback c
            return Nothing
        False -> return Nothing


  e -> do
    return Nothing
    -- lift $ liftIO $ putStrLn $ "UNCAUGHT EVENT: " ++ (show e)
    -- return $ Just (CE.SomeException $ CE.PatternMatchFail "processEvent")
  where
    -- Assumes handle is set
    send :: String -> Either Handle TLSCtx -> IO ()
    send s o = case o of
      Left handle -> do
        -- liftIO $ hPutStr handle $ encodeString $ s
        -- liftIO $ hFlush handle
        return ()
      Right tlsCtx -> do
        liftIO $ sendData tlsCtx $ DBLC.pack $ encodeString s
        return ()
    shouldIgnoreIQ :: IQ -> Bool
    shouldIgnoreIQ i = case iqPayload i of
      Nothing -> False
      Just e -> case nameNamespace $ elementName e of
        Just x | x == DT.pack "urn:ietf:params:xml:ns:xmpp-bind" -> True
        Just x | x == DT.pack "urn:ietf:params:xml:ns:xmpp-session" -> True
        Just _ -> False
        Nothing -> False


registerTimeout :: (ClientState s m, MonadIO m) => Chan (InternalEvent s m) -> StanzaID -> Timeout -> StateT s m () -> StateT (State s m) m ()
registerTimeout ch i t ca = do
    liftIO $ threadDelay $ t * 1000
    liftIO $ forkIO $ writeChan ch $ IET (TimeoutEvent i t ca)
    return ()


runBoolClientCallback :: (ClientState s m, MonadIO m) => StateT s m Bool -> StateT (State s m) m Bool
runBoolClientCallback c = do
    state <- get
    let clientState = stateClientState state
    (bool, clientState') <- lift $ runStateT c clientState
    put $ state { stateClientState = clientState' }
    return bool


runUnitClientCallback :: (ClientState s m, MonadIO m) => StateT s m () -> StateT (State s m) m ()
runUnitClientCallback c = do
    state <- get
    let clientState = stateClientState state
    ((), clientState') <- lift $ runStateT c clientState
    put $ state { stateClientState = clientState' }


sendToClient :: (MonadIO m, ClientState s m) => [Maybe (StateT s m Bool)] -> s -> (StateT (State s m) m) s
sendToClient [] s = return s
sendToClient (Nothing:fs) s = sendToClient fs s
sendToClient ((Just f):fs) s = do
  (b, s') <- lift $ runStateT f s
  case b of
    True -> return s'
    False -> sendToClient fs s'
