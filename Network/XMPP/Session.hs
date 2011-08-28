-- Copyright © 2010-2011 Jon Kristensen. See the LICENSE file in the Pontarius
-- XMPP distribution for more details.


-- TODO: Predicates on callbacks?
-- TODO: . vs $


module Network.XMPP.NewSession (
    XMPPT (runXMPPT)
) where


-- |
-- The XMPP monad transformer. XMPP clients will need to operate in this
-- context.

data XMPPT m a = XMPPT { runXMPPT :: XMPPT m a -> m a
                       , internalEventChan :: Chan InternalEvent
                       , hookModificationsChan :: Chan HookModification }


-- |
-- Events that may be emitted from Pontarius XMPP.

data Event
    = ConnectedEvent (Either ConnectionFailureReason Resource)
    -- | OpenedStreamsEvent (Maybe OpenStreamsFailureReason)
    -- | TLSSecuredEvent (Maybe TLSSecuringFailureReason)
    -- | AuthenticatedEvent (Either AuthenticationFailureReason Resource)
    -- | DisconnectedEvent DisconnectReason
    -- | MessageEvent (Either MessageError Message)
    -- | PresenceEvent (Either PresenceError Presence)
    -- | IQEvent (Either IQResult IQRequest)
    | forall a. Dynamic a => DynamicEvent a
    deriving (Show)


data ConnectedFailureReason
    = COSFR -- OpenStreamFailureReason
    | CTSFR -- TLSSecureFailureReason
    | CAFR -- AuthenticateFailureReason


-- data OpenStreamFailureReason

-- data TLSSecureFailureReason

-- data AuthenticateFailureReason


-- Internal events processed in the main state loop of Pontarius XMPP. They are
-- either received from the client or from the enumerator.

data InternalEvent
    = IECE ClientEvent
    | IEEE EnumeratorEvent


-- The "hook modification" events have a higher priority than other events, and
-- are thus sent through a Chan of their own. The boolean returns value signals
-- whether or not the hook should be removed.

data HookModification m
    = RegisterConnectedHook (ConnectedEvent -> XMPPT m Bool)
    | forall a. Dynamic a => RegisterDynamicHook (DynamicEvent a -> XMPPT m Bool)


stateLoop :: State -> Chan InternalEvent -> IO ()

stateLoop s c = do
    ie <- readChan c
    let (s', ios) = processInternalEvent s ie in
        -- forall ios, execute it
        stateLoop s' c


processInternalEvent :: State -> InternalEvent -> (State, [IO ()])

processInternalEvent s ie = (s, [connectIO])

    where

        connectIO :: IO ()
        connectIO = return ()


hookConnectedEvent :: (ConnectedEvent -> XMPPT m Bool) -> XMPPT m ()

hookConnectedEvent = writeChan hookModificationsChan . RegisterConnectedHook


hookDynamicEvent :: Dynamic a => (DynamicEvent a -> XMPPT m Bool) -> XMPPT m ()

hookDynamicEvent h = writeChan hookModificationsChan . RegisterDynamicHook


hookStreamOpenedEvent :: (StreamOpenedEvent -> XMPPT m Bool) -> XMPPT m ()

hookStreamOpenedEvent = writeChan hookModificationsChan . RegisterStreamOpenedHook


hookTLSSecuredEvent :: (TLSSecuredEvent -> XMPPT m Bool) -> XMPPT m ()

hookTLSSecuredEvent = writeChan hookModificationsChan . RegisterTLSSecuredHook


hookAuthenticatedEvent :: (AuthenticatedEvent -> XMPPT m Bool) -> XMPPT m ()

hookAuthenticatedEvent = writeChan hookModificationsChan . RegisterAuthenticatedHook


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

connect :: HostName -> PortNumber -> Maybe (Maybe [X509], ([X509] -> Bool)) -> Maybe (UserName, Password, Maybe Resource) -> XMPPT m ()

connect h p Nothing Nothing = do
    hookStreamOpenedEvent onStreamOpenedEvent Nothing
    openStream h p

    where

        onStreamOpenedEvent Nothing = do
            fireConnectedEvent Nothing
            return False

        onStreamOpenedEvent (Just e) = do
            fireConnectedEvent $ ConnectedFailureReason $ COSFR e
            return False

connect h p (Just t) Nothing = do
    hookStreamOpenedEvent onStreamOpenedEvent Nothing
    openStream h p

    where

        onStreamOpenedEvent Nothing = do
            hookTLSSecuredEvent onTLSSecuredEvent Nothing
            tlsSecure
            return False

        onStreamOpenedEvent (Just e) = do
            fireConnectedEvent $ ConnectedFailureReason $ COSFR e
            return False

        onTLSSecuredEvent Nothing = do
            fireConnectedEvent Nothing
            return False

        onTLSSecuredEvent (Just e) = do
            fireConnectedEvent $ ConnectedFailureReason $ CTSFR e
            return False

connect h p Nothing (Just a) = do
    hookStreamOpenedEvent onStreamOpenedEvent Nothing
    openStream h p

    where

        onStreamOpenedEvent Nothing = do
            hookAuthenticatedEvent onAuthenticatedEvent Nothing
            authenticate
            return False

        onStreamOpenedEvent (Just e) = do
            fireConnectedEvent $ ConnectedFailureReason $ COSFR e
            return False

        onAuthenticatedEvent (Right r) = do
            fireConnectedEvent $ Just r
            return False

        onAuthenticated (Left e) = do
            fireConnectedEvent $ ConnectedFailureReason $ CAFR e
            return False

connect h p (Just t) (Just a) = do
    hookStreamOpenedEvent onStreamOpenedEvent Nothing
    openStream h p

    where

        onStreamOpenedEvent Nothing = do
            hookTLSSecuredEvent onTLSSecuredEvent Nothing
            tlsSecure
            return False

        onStreamOpenedEvent (Just e) = do
            fireConnectedEvent $ ConnectedFailureReason $ COSFR e
            return False

        onTLSSecuredEvent Nothing = do
            hookAuthenticatedEvent onAuthenticatedEvent Nothing
            authenticate
            return False

        onTLSSecuredEvent (Just e) = do
            fireConnectedEvent $ ConnectedFailureReason $ CTSFR e
            return False

        onAuthenticatedEvent (Right r) = do
            fireConnectedEvent $ Just r
            return False

        onAuthenticated (Left e) = do
            fireConnectedEvent $ ConnectedFailureReason $ CAFR e
            return False
