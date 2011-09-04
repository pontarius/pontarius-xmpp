-- Copyright © 2010-2011 Jon Kristensen. See the LICENSE file in the Pontarius
-- XMPP distribution for more details.


-- TODO: Predicates on callbacks?
-- TODO: . vs $
-- TODO: type XMPP = XMPPT IO? + runXMPP


{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}


module Network.XMPP.Session (
    XMPPT (runXMPPT)
) where


import Network.XMPP.Types

import Control.Concurrent (Chan, readChan, writeChan)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Certificate.X509 (X509)
import Data.Dynamic (Dynamic)
import Control.Monad.Reader (MonadReader, ReaderT, ask)


-- |
-- The XMPP monad transformer. XMPP clients will need to operate in this
-- context.

newtype XMPPT m a
    = XMPPT { runXMPPT :: ReaderT (ReaderState m) m a }
    deriving (Monad, MonadIO)

deriving instance (Monad m, MonadIO m) => MonadReader (ReaderState m) (XMPPT m)


data ReaderState m = ReaderState { intEvtChan :: Chan InternalEvent
                                 , hookModChan :: Chan (HookModification m) }


-- |
-- Events that may be emitted from Pontarius XMPP.

data ConnectedEvent = ConnectedEvent (Either ConnectedFailureReason Resource)

-- data DynamicEvent = forall a. Dynamic a => DynamicEvent a
data DynamicEvent = DynamicEvent Dynamic

type OpenedStreamsEvent = Maybe OpenStreamsFailureReason

type TLSSecuredEvent = Maybe TLSSecureFailureReason

type AuthenticatedEvent = Either AuthenticateFailureReason Resource

--data Event
--    = ConnectedEvent (Either IntFailureReason Resource)
--    -- | OpenedStreamsEvent (Maybe OpenStreamsFailureReason)
--    -- | TLSSecuredEvent (Maybe TLSSecuringFailureReason)
--    -- | AuthenticatedEvent (Either AuthenticationFailureReason Resource)
--    -- | DisconnectEvent DisconnectReason
--    -- | MessageEvent (Either MessageError Message)
--    -- | PresenceEvent (Either PresenceError Presence)
--    -- | IQEvent (Either IQResult IQRequest)
--    | forall a. Dynamic a => DynamicEvent a
--    deriving (Show)


data ConnectedFailureReason
    = COSFR OpenStreamsFailureReason
    | CTSFR TLSSecureFailureReason
    | CAFR AuthenticateFailureReason


data OpenStreamsFailureReason = OpenStreamFailureReason

data TLSSecureFailureReason = TLSSecureFailureReason

data AuthenticateFailureReason = AuthenticateFailureReason


-- The "hook modification" events have a higher priority than other events, and
-- are thus sent through a Chan of their own. The boolean returns value signals
-- whether or not the hook should be removed.

data HookModification m
    = MonadIO m => RegisterConnectedHook (ConnectedEvent -> XMPPT m Bool) (Maybe (ConnectedEvent -> Bool))
    | RegisterStreamsOpenedHook (OpenedStreamsEvent -> XMPPT m Bool) (Maybe (OpenedStreamsEvent -> Bool))
    | RegisterTLSSecuredHook (TLSSecuredEvent -> XMPPT m Bool) (Maybe (TLSSecuredEvent -> Bool))
    | RegisterAuthenticatedHook (AuthenticatedEvent -> XMPPT m Bool) (Maybe (AuthenticatedEvent -> Bool))
    -- | forall a. Dynamic a => RegisterDynamicHook (DynamicEvent a -> XMPPT m Bool)
    | RegisterDynamicHook (DynamicEvent -> XMPPT m Bool) (Maybe (DynamicEvent -> Bool))


data State = State


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


hookConnectedEvent :: MonadIO m => (ConnectedEvent -> XMPPT m Bool) -> (Maybe (ConnectedEvent -> Bool)) -> XMPPT m ()

hookConnectedEvent cb pred = ask >>= \rs -> liftIO $ writeChan (hookModChan rs) (RegisterConnectedHook cb pred)


hookDynamicEvent :: MonadIO m => (DynamicEvent -> XMPPT m Bool) -> (Maybe (DynamicEvent -> Bool)) -> XMPPT m ()

hookDynamicEvent cb pred = ask >>= \rs -> liftIO $ writeChan (hookModChan rs) (RegisterDynamicHook cb pred)


hookStreamsOpenedEvent :: MonadIO m => (OpenedStreamsEvent -> XMPPT m Bool) -> (Maybe (OpenedStreamsEvent -> Bool)) -> XMPPT m ()

hookStreamsOpenedEvent cb pred = ask >>= \rs -> liftIO $ writeChan (hookModChan rs) (RegisterStreamsOpenedHook cb pred)


hookTLSSecuredEvent :: MonadIO m => (TLSSecuredEvent -> XMPPT m Bool) -> (Maybe (TLSSecuredEvent -> Bool)) -> XMPPT m ()

hookTLSSecuredEvent cb pred = ask >>= \rs -> liftIO $ writeChan (hookModChan rs) (RegisterTLSSecuredHook cb pred)


hookAuthenticatedEvent :: MonadIO m => (AuthenticatedEvent -> XMPPT m Bool) -> (Maybe (AuthenticatedEvent -> Bool)) -> XMPPT m ()

hookAuthenticatedEvent cb pred = ask >>= \rs -> liftIO $ writeChan (hookModChan rs) (RegisterAuthenticatedHook cb pred)


openStreams = openStreams
tlsSecure = tlsSecure
authenticate = authenticate


fireConnectedEvent = fireConnectedEvent

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

connect :: MonadIO m => HostName -> PortNumber -> Maybe (Maybe [X509], ([X509] -> Bool)) -> Maybe (UserName, Password, Maybe Resource) -> XMPPT m ()

connect h p Nothing Nothing = do
    hookStreamsOpenedEvent onStreamsOpenedEvent Nothing
    openStreams h p

    where

        onStreamsOpenedEvent Nothing = do
            fireConnectedEvent Nothing
            return False

        onStreamsOpenedEvent (Just e) = do
            fireConnectedEvent $ Left $ COSFR e
            return False

connect h p (Just t) Nothing = do
    hookStreamsOpenedEvent onStreamsOpenedEvent Nothing
    openStreams h p

    where

        onStreamsOpenedEvent Nothing = do
            hookTLSSecuredEvent onTLSSecuredEvent Nothing
            tlsSecure
            return False

        onStreamsOpenedEvent (Just e) = do
            fireConnectedEvent $ Left $ COSFR e
            return False

        onTLSSecuredEvent Nothing = do
            fireConnectedEvent Nothing
            return False

        onTLSSecuredEvent (Just e) = do
            fireConnectedEvent $ Left $ CTSFR e
            return False

connect h p Nothing (Just a) = do
    hookStreamsOpenedEvent onStreamsOpenedEvent Nothing
    openStreams h p

    where

        onStreamsOpenedEvent Nothing = do
            hookAuthenticatedEvent onAuthenticatedEvent Nothing
            authenticate
            return False

        onStreamsOpenedEvent (Just e) = do
            fireConnectedEvent $ Left $ COSFR e
            return False

        onAuthenticatedEvent (Right r) = do
            fireConnectedEvent $ Just r
            return False

        onAuthenticated (Left e) = do
            fireConnectedEvent $ Left $ CAFR e
            return False

connect h p (Just t) (Just a) = do
    hookStreamsOpenedEvent onStreamsOpenedEvent Nothing
    openStreams h p

    where

        onStreamsOpenedEvent Nothing = do
            hookTLSSecuredEvent onTLSSecuredEvent Nothing
            tlsSecure
            return False

        onStreamsOpenedEvent (Just e) = do
            fireConnectedEvent $ Left $ COSFR e
            return False

        onTLSSecuredEvent Nothing = do
            hookAuthenticatedEvent onAuthenticatedEvent Nothing
            authenticate
            return False

        onTLSSecuredEvent (Just e) = do
            fireConnectedEvent $ Left $ CTSFR e
            return False

        onAuthenticatedEvent (Right r) = do
            fireConnectedEvent $ Just r
            return False

        onAuthenticated (Left e) = do
            fireConnectedEvent $ Left $ CAFR e
            return False
