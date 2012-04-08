-- Copyright © 2010-2012 Jon Kristensen. See the LICENSE file in the
-- Pontarius distribution for more details.


-- TODO: Predicates on callbacks?
-- TODO: . vs $
-- TODO: type XMPP = XMPPT IO? + runXMPP


{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MultiParamTypeClasses #-}


module Network.XMPP.Session (
    XMPPT (runXMPPT)
    , hookStreamsOpenedEvent
      , hookDisconnectedEvent
        , destroy
          , openStreams
            , create
            , DisconnectReason
) where

import Network.XMPP.Stream
import Network.XMPP.Types
import Network.XMPP.Utilities

import Control.Concurrent (Chan, forkIO, forkOS, newChan, readChan, writeChan)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Certificate.X509 (X509)
import Data.Dynamic (Dynamic)
-- import Control.Monad.Reader (MonadReader, ReaderT, ask)
import Control.Monad.State.Lazy (MonadState, StateT, get, put, execStateT)

import qualified Control.Exception as CE
import qualified Network as N
import System.IO (BufferMode, BufferMode(NoBuffering))
import GHC.IO.Handle (Handle, hPutStr, hFlush, hSetBuffering, hWaitForInput)
import Codec.Binary.UTF8.String



create :: MonadIO m => XMPPT m () -> m ()

create main = do
  chan <- liftIO $ newChan
  idGen <- liftIO $ idGenerator ""
  execStateT (runXMPPT init) (State chan idGen [])
  return ()
  where
    init = do
      main
      stateLoop


-- Internal events - events to be processed within Pontarius.

-- data InternalEvent s m = IEC (ClientEvent s m) | IEE EnumeratorEvent | IET (TimeoutEvent s m) deriving (Show)


instance Show (InternalEvent m) where
  show _ = "InternalEvent"

-- |
-- Events that may be emitted from Pontarius.

data Event = -- ConnectedEvent (Either IntFailureReason Resource)
           {-|-} OpenedStreamsEvent (Maybe OpenStreamsFailureReason)
     -- | TLSSecuredEvent (Maybe TLSSecuringFailureReason)
     -- | AuthenticatedEvent (Either AuthenticationFailureReason Resource)
     | DisconnectedEvent DisconnectReason
     -- | MessageEvent (Either MessageError Message)
     -- | PresenceEvent (Either PresenceError Presence)
     -- | IQEvent (Either IQResult IQRequest)
     -- | forall a. Dynamic a => DynamicEvent a
     deriving (Show)

-- data DynamicEvent = forall a. Dynamic a => DynamicEvent a
-- data DynamicEvent = DynamicEvent Dynamic


-- data ConnectedFailureReason
--     = COSFR OpenStreamsFailureReason
--     | CTSFR TLSSecureFailureReason
--     | CAFR AuthenticateFailureReason


-- The "hook modification" events have a higher priority than other events, and
-- are thus sent through a Chan of their own. The boolean returns value signals
-- whether or not the hook should be removed.

-- data HookModification m
--     = MonadIO m => -- RegisterConnectedHook (ConnectedEvent -> XMPPT m Bool) (Maybe (ConnectedEvent -> Bool))
    -- | RegisterTLSSecuredHook (TLSSecuredEvent -> XMPPT m Bool) (Maybe (TLSSecuredEvent -> Bool))
    -- | RegisterAuthenticatedHook (AuthenticatedEvent -> XMPPT m Bool) (Maybe (AuthenticatedEvent -> Bool))
    -- -- | forall a. Dynamic a => RegisterDynamicHook (DynamicEvent a -> XMPPT m Bool)
    -- | RegisterDynamicHook (DynamicEvent -> XMPPT m Bool) (Maybe (DynamicEvent -> Bool))


-- Reads an event from the internal event channel, processes it,
-- performs the generated impure actions, and loops.

stateLoop :: MonadIO m => XMPPT m ()

stateLoop = do
  rs <- get
  event <- liftIO $ readChan $ evtChan rs
  liftIO $ putStrLn $ "Processing " ++ (show event) ++ "..."
  processEvent event
  -- sequence_ IO actions frmo procesEvent?
  stateLoop


-- Processes an internal event and generates a list of impure actions.

processEvent :: MonadIO m => InternalEvent m -> XMPPT m ()

processEvent (OpenStreamsEvent h p) = openStreamAction h p
  where
    openStreamAction :: MonadIO m => HostName -> PortNumber -> XMPPT m ()
    openStreamAction h p = let p' = fromIntegral p
                               computation chan = do -- chan ugly
                                 -- threadID <-
                                 handle <- N.connectTo h (N.PortNumber p')
                                 hSetBuffering handle NoBuffering
                                 forkIO $ conduit chan (Left handle) -- This must be done after hSetBuffering
                                 hPutStr handle $ encodeString "<stream:stream to='" ++ h ++ "' version='1.0' xmlns='jabber:client' xmlns:stream='http://etherx.jabber.org/streams'>" -- didn't work with <?xml version='1.0'>
                                 hFlush handle
                                 return ()
                           in do
                             rs <- get
                             result <- liftIO $ CE.try (computation $ evtChan rs)
                             case result of
                               Right () -> do
                                 return ()
                                 -- -- lift $ liftIO $ putMVar (stateThreadID state) threadID
                               Left (CE.SomeException e) -> do -- TODO: Safe to do this?
                                 fireStreamsOpenedEvent $ Just OpenStreamsFailureReason
                                 -- Left error -> do
                                 -- -- let clientState = stateClientState state
                                 -- -- ((), clientState') <- lift $ runStateT (callback OpenStreamFailure) clientState
                                 -- -- put $ state { stateShouldExit = True }
                                 -- -- return $ Just e
                                 -- return $ Just error


-- hookConnectedEvent :: MonadIO m => (ConnectedEvent -> XMPPT m Bool) -> (Maybe (ConnectedEvent -> Bool)) -> XMPPT m ()

-- hookConnectedEvent cb pred = ask >>= \rs -> liftIO $ writeChan (hookModChan rs) (RegisterConnectedHook cb pred)


-- | Hook the provided callback and (optional) predicate to the
--   "Streams Opened" event.
--   
--   The "Streams Opened" event will be fired when the stream:features element has been successfully received or an error has occurred.

hookStreamsOpenedEvent :: MonadIO m => (Maybe OpenStreamsFailureReason -> XMPPT m Bool) -> (Maybe (Maybe OpenStreamsFailureReason -> XMPPT m Bool)) -> XMPPT m HookId

hookStreamsOpenedEvent cb pred = do
  rs <- get
  hookId <- liftIO $ nextId $ hookIdGenerator rs
  put $ rs { hooks = (HookId hookId, StreamsOpenedHook pred cb):hooks rs }
  return $ HookId hookId


hookDisconnectedEvent :: MonadIO m => (DisconnectReason -> XMPPT m Bool) -> (Maybe (DisconnectReason -> XMPPT m Bool)) -> XMPPT m HookId
hookDisconnectedEvent cb pred = do
  rs <- get
  hookId <- liftIO $ nextId $ hookIdGenerator rs  
  -- TODO: Actually hook it.
  return $ HookId hookId


-- hookTLSSecuredEvent :: MonadIO m => (TLSSecuredEvent -> XMPPT m Bool) -> (Maybe (TLSSecuredEvent -> Bool)) -> XMPPT m ()

-- hookTLSSecuredEvent cb pred = ask >>= \rs -> liftIO $ writeChan (hookModChan rs) (RegisterTLSSecuredHook cb pred)


-- hookAuthenticatedEvent :: MonadIO m => (AuthenticatedEvent -> XMPPT m Bool) -> (Maybe (AuthenticatedEvent -> Bool)) -> XMPPT m ()

-- hookAuthenticatedEvent cb pred = ask >>= \rs -> liftIO $ writeChan (hookModChan rs) (RegisterAuthenticatedHook cb pred)


-- hookDynamicEvent :: MonadIO m => (DynamicEvent -> XMPPT m Bool) -> (Maybe (DynamicEvent -> Bool)) -> XMPPT m ()

-- hookDynamicEvent cb pred = ask >>= \rs -> liftIO $ writeChan (hookModChan rs) (RegisterDynamicHook cb pred)


-- | Asynchronously request to open a stream to an XMPP server on the
--   given host name and port.

openStreams :: MonadIO m => HostName -> PortNumber -> XMPPT m ()

openStreams h p = get >>= \rs -> liftIO $ writeChan (evtChan rs) (OpenStreamsEvent h p)


-- Like any other fire*Event function, it queries the hooks, filters
-- out the ones that are relevant, prepares them to be used with
-- processHook, and processes them.

fireStreamsOpenedEvent :: MonadIO m => Maybe OpenStreamsFailureReason -> XMPPT m ()

fireStreamsOpenedEvent r = do
  rs <- get
  let hooks' = filterStreamsOpenedHooks $ hooks rs
  sequence_ $ map (\(hookId, pred, cb) -> processHook hookId pred cb) $ map prepareStreamsOpenedHooks hooks'
  return ()
    where
      prepareStreamsOpenedHooks :: MonadIO m => Hook m -> (HookId, Maybe (XMPPT m Bool), XMPPT m Bool)
      prepareStreamsOpenedHooks (hookId, StreamsOpenedHook pred cb) =
        let pred' = case pred of
              Nothing -> Nothing
              Just pred'' -> Just $ pred'' r
            cb' = cb r in (hookId, pred', cb')


-- Takes an optional predicate and a callback function, and excecutes
-- the callback function if the predicate does not exist, or exists
-- and is true, and returns True if the hook should be removed.

processHook :: MonadIO m => HookId -> Maybe (XMPPT m Bool) -> XMPPT m Bool -> XMPPT m ()

processHook id pred cb = do
  remove <- processHook'
  if remove then do
    rs <- get
    put $ rs { hooks = removeHook id (hooks rs) }
  else return ()
  where
    processHook' = case pred of
      Just pred' -> do
        result <- pred'
        if result then cb else return False
      Nothing -> cb


destroy = destroy


filterStreamsOpenedHooks :: MonadIO m => [Hook m] -> [Hook m]

filterStreamsOpenedHooks h = filter pred h
  where
    pred (_, StreamsOpenedHook _ _) = True
    pred _ = False


removeHook :: MonadIO m => HookId -> [Hook m] -> [Hook m]

removeHook id h = filter (\(id', _) -> id' /= id) h


-- tlsSecure = tlsSecure

-- authenticate = authenticate


-- fireConnectedEvent = fireConnectedEvent


-- |
-- connect is implemented using hookStreamOpenedEvent, hookTLSSecuredEvent, and
-- hookAuthenticatedEvent, and is offered as a convenience function for clients
-- that doesn't need to perform any XMPP actions in-between opening the streams
-- and TLS securing the stream and\/or authenticating, allowing them to listen
-- for and manage one event instead of up to three. Just-values in the third and
-- fourth parameters will make connect TLS secure the stream and authenticate,
-- respectively. Most clients will want to hook to the Connected event using
-- hookConnectedEvent prior to using this function.
--
-- The ConnectedEvent and StreamOpenedEvent are guaranteed to be generated upon
-- calling this function. So will a subset of the TLSSecuredEvent and
-- AuthenticatedEvent, depending on whether their functionalities are requested
-- using Just-values in the third and fourth parameters.
--
-- connect is designed with the assupmtion that openStreams, tlsSecure, and
-- authenticate will not be used by the client. Calling those functions may
-- generate events that can cause connect to behave incorrectly.

-- connect :: MonadIO m => HostName -> PortNumber -> Maybe (Maybe [X509], ([X509] -> Bool)) -> Maybe (UserName, Password, Maybe Resource) -> XMPPT m ()
-- 
-- connect h p Nothing Nothing = do
--     hookStreamsOpenedEvent onStreamsOpenedEvent Nothing
--     openStreams h p
-- 
--     where
-- 
--         onStreamsOpenedEvent Nothing = do
--             fireConnectedEvent Nothing
--             return False
-- 
--         onStreamsOpenedEvent (Just e) = do
--             fireConnectedEvent $ Left $ COSFR e
--             return False
-- 
-- connect h p (Just t) Nothing = do
--     hookStreamsOpenedEvent onStreamsOpenedEvent Nothing
--     openStreams h p
-- 
--     where
-- 
--         onStreamsOpenedEvent Nothing = do
--             hookTLSSecuredEvent onTLSSecuredEvent Nothing
--             tlsSecure
--             return False
-- 
--         onStreamsOpenedEvent (Just e) = do
--             fireConnectedEvent $ Left $ COSFR e
--             return False
-- 
--         onTLSSecuredEvent Nothing = do
--             fireConnectedEvent Nothing
--             return False
-- 
--         onTLSSecuredEvent (Just e) = do
--             fireConnectedEvent $ Left $ CTSFR e
--             return False
-- 
-- connect h p Nothing (Just a) = do
--     hookStreamsOpenedEvent onStreamsOpenedEvent Nothing
--     openStreams h p
-- 
--     where
-- 
--         onStreamsOpenedEvent Nothing = do
--             hookAuthenticatedEvent onAuthenticatedEvent Nothing
--             authenticate
--             return False
-- 
--         onStreamsOpenedEvent (Just e) = do
--             fireConnectedEvent $ Left $ COSFR e
--             return False
-- 
--         onAuthenticatedEvent (Right r) = do
--             fireConnectedEvent $ Just r
--             return False
-- 
--         onAuthenticated (Left e) = do
--             fireConnectedEvent $ Left $ CAFR e
--             return False
-- 
-- connect h p (Just t) (Just a) = do
--     hookStreamsOpenedEvent onStreamsOpenedEvent Nothing
--     openStreams h p
-- 
--     where
-- 
--         onStreamsOpenedEvent Nothing = do
--             hookTLSSecuredEvent onTLSSecuredEvent Nothing
--             tlsSecure
--             return False
-- 
--         onStreamsOpenedEvent (Just e) = do
--             fireConnectedEvent $ Left $ COSFR e
--             return False
-- 
--         onTLSSecuredEvent Nothing = do
--             hookAuthenticatedEvent onAuthenticatedEvent Nothing
--             authenticate
--             return False
-- 
--         onTLSSecuredEvent (Just e) = do
--             fireConnectedEvent $ Left $ CTSFR e
--             return False
-- 
--         onAuthenticatedEvent (Right r) = do
--             fireConnectedEvent $ Just r
--             return False
-- 
--         onAuthenticated (Left e) = do
--             fireConnectedEvent $ Left $ CAFR e
--             return False