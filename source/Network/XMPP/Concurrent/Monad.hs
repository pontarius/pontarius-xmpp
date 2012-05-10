module Network.XMPP.Concurrent.Monad where

import           Network.XMPP.Types

import           Control.Concurrent
import           Control.Concurrent.STM
import qualified Control.Exception.Lifted as Ex
import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Control.Monad.State.Strict

import           Data.IORef
import qualified Data.Map as Map
import           Data.Text(Text)

import           Network.XMPP.Concurrent.Types
import           Network.XMPP.Monad


-- | Register a new IQ listener. IQ requests matching the type and namespace
-- will be put in the channel.
--
-- Return the new channel or Nothing if this namespace/'IQRequestType'
-- combination was alread handled.
listenIQChan :: IQRequestType  -- ^ Type of IQs to receive (@Get@ or @Set@)
             -> Text -- ^ Namespace of the child element
             -> XMPP (Maybe (TChan IQRequestTicket))
listenIQChan tp ns = do
    handlers <- asks iqHandlers
    liftIO . atomically $ do
        (byNS, byID) <- readTVar handlers
        iqCh <- newTChan
        let (present, byNS') = Map.insertLookupWithKey'
                (\_ _ old -> old)
                (tp, ns)
                iqCh
                byNS
        writeTVar handlers (byNS', byID)
        return $ case present of
            Nothing -> Just iqCh
            Just _iqCh' -> Nothing

-- | Get a duplicate of the stanza channel
getStanzaChan :: XMPP (TChan Stanza)
getStanzaChan = do
    shadow <- asks sShadow
    liftIO $ atomically $ dupTChan shadow

-- | Get the inbound stanza channel, duplicates from master if necessary. Please
-- note that once duplicated it will keep filling up, call 'dropMessageChan' to
-- allow it to be garbage collected.
getMessageChan :: XMPP (TChan (Either MessageError Message))
getMessageChan = do
    mChR <- asks messagesRef
    mCh <- liftIO $ readIORef mChR
    case mCh of
        Nothing -> do
            shadow <- asks mShadow
            mCh' <- liftIO $ atomically $ dupTChan shadow
            liftIO $ writeIORef mChR (Just mCh')
            return mCh'
        Just mCh' -> return mCh'

-- | Analogous to 'getMessageChan'.
getPresenceChan :: XMPP (TChan (Either PresenceError Presence))
getPresenceChan = do
    pChR <- asks presenceRef
    pCh <- liftIO $ readIORef pChR
    case pCh of
        Nothing -> do
            shadow <- asks pShadow
            pCh' <- liftIO $ atomically $ dupTChan shadow
            liftIO $ writeIORef pChR (Just pCh')
            return pCh'
        Just pCh' -> return pCh'

-- | Drop the local end of the inbound stanza channel from our context so it can
-- be GC-ed.
dropMessageChan :: XMPP ()
dropMessageChan = do
    r <- asks messagesRef
    liftIO $ writeIORef r Nothing

-- | Analogous to 'dropMessageChan'.
dropPresenceChan :: XMPP ()
dropPresenceChan = do
    r <- asks presenceRef
    liftIO $ writeIORef r Nothing

-- | Read an element from the inbound stanza channel, acquiring a copy of the
-- channel as necessary.
pullMessage :: XMPP (Either MessageError Message)
pullMessage = do
    c <- getMessageChan
    liftIO $ atomically $ readTChan c

-- | Read an element from the inbound stanza channel, acquiring a copy of the
-- channel as necessary.
pullPresence :: XMPP (Either PresenceError Presence)
pullPresence = do
    c <- getPresenceChan
    liftIO $ atomically $ readTChan c

-- | Send a stanza to the server.
sendStanza :: Stanza -> XMPP ()
sendStanza a = do
    out <- asks outCh
    liftIO . atomically $ writeTChan out a
    return ()

-- | Create a forked session object without forking a thread.
forkSession :: Session -> IO Session
forkSession sess = do
    mCH' <- newIORef Nothing
    pCH' <- newIORef Nothing
    return $ sess {messagesRef = mCH', presenceRef = pCH'}

-- | Fork a new thread.
fork :: XMPP () -> XMPP ThreadId
fork a = do
    sess <- ask
    sess' <- liftIO $ forkSession sess
    liftIO $ forkIO $ runReaderT a sess'

-- | Pulls a message and returns it if the given predicate returns @True@.
filterMessages :: (MessageError -> Bool)
               -> (Message -> Bool)
               -> XMPP (Either MessageError Message)
filterMessages f g = do
    s <- pullMessage
    case s of
        Left  e | f e -> return $ Left e
                | otherwise -> filterMessages f g
        Right m | g m -> return $ Right m
                | otherwise -> filterMessages f g

-- | Pulls a (non-error) message and returns it if the given predicate returns
-- @True@.
waitForMessage :: (Message -> Bool) -> XMPP Message
waitForMessage f = do
    s <- pullMessage
    case s of
        Left _ -> waitForMessage f
        Right m | f m -> return m
                | otherwise -> waitForMessage f

-- | Pulls an error message and returns it if the given predicate returns @True@.
waitForMessageError :: (MessageError -> Bool) -> XMPP MessageError
waitForMessageError f = do
    s <- pullMessage
    case s of
        Right _ -> waitForMessageError f
        Left  m | f m -> return m
                | otherwise -> waitForMessageError f

-- | Pulls a (non-error) presence and returns it if the given predicate returns
-- @True@.
waitForPresence :: (Presence -> Bool) -> XMPP Presence
waitForPresence f = do
    s <- pullPresence
    case s of
        Left _ -> waitForPresence f
        Right m | f m -> return m
                | otherwise -> waitForPresence f

-- TODO: Wait for presence error?

-- | Run an XMPPMonad action in isolation. Reader and writer workers will be
-- temporarily stopped and resumed with the new session details once the action
-- returns. The action will run in the calling thread. Any uncaught exceptions
-- will be interpreted as connection failure.
withConnection :: XMPPConMonad a -> XMPP (Either StreamError a)
withConnection a = do
    readerId <- asks readerThread
    stateRef <- asks conStateRef
    write <- asks writeRef
    wait <- liftIO $ newEmptyTMVarIO
    liftIO . Ex.mask_ $ do
        -- Suspends the reader until the lock (wait) is released (set to `()').
        throwTo readerId $ Interrupt wait
        -- We acquire the write and stateRef locks, to make sure that this is
        -- the only thread that can write to the stream and to perform a
        -- withConnection calculation. Afterwards, we release the lock and
        -- fetches an updated state.
        s <- Ex.catch
            (atomically $ do
                 _ <- takeTMVar write
                 s <- takeTMVar stateRef
                 putTMVar wait ()
                 return s
            )
            -- If we catch an exception, we have failed to take the MVars above.
            (\e -> atomically (putTMVar wait ()) >>
                 Ex.throwIO (e :: Ex.SomeException)
            )
        -- Run the XMPPMonad action, save the (possibly updated) states, release
        -- the locks, and return the result.
        Ex.catches
            (do
                 (res, s') <- runStateT a s
                 atomically $ do
                     putTMVar write (sConPushBS s')
                     putTMVar stateRef s'
                     return $ Right res
            )
            -- We treat all Exceptions as fatal. If we catch a StreamError, we
            -- return it. Otherwise, we throw an exception.
            [ Ex.Handler $ \e -> return $ Left (e :: StreamError)
            , Ex.Handler $ \e -> runStateT xmppKillConnection s
                  >> Ex.throwIO (e :: Ex.SomeException)
            ]

-- | Send a presence stanza.
sendPresence :: Presence -> XMPP ()
sendPresence = sendStanza . PresenceS

-- | Send a message stanza.
sendMessage :: Message -> XMPP ()
sendMessage = sendStanza . MessageS

-- | Executes a function to update the event handlers.
modifyHandlers :: (EventHandlers -> EventHandlers) -> XMPP ()
modifyHandlers f = do
    eh <- asks eventHandlers
    liftIO . atomically $ writeTVar eh . f =<< readTVar eh

-- | Sets the handler to be executed when the session ends.
setSessionEndHandler :: XMPP () -> XMPP ()
setSessionEndHandler eh = do
    r <- ask
    modifyHandlers (\s -> s{sessionEndHandler = runReaderT eh r})

-- | Sets the handler to be executed when the server connection is closed.
setConnectionClosedHandler :: (StreamError -> XMPP ()) -> XMPP ()
setConnectionClosedHandler eh = do
    r <- ask
    modifyHandlers (\s -> s{connectionClosedHandler = \e -> runReaderT (eh e) r})

-- | Run an event handler.
runHandler :: (EventHandlers -> IO a) -> XMPP a
runHandler h = do
  eh <- liftIO . atomically . readTVar  =<< asks eventHandlers
  liftIO $ h eh

-- | End the current XMPP session.
endSession :: XMPP ()
endSession = do -- TODO: This has to be idempotent (is it?)
    void $ withConnection xmppKillConnection
    liftIO =<< asks stopThreads
    runHandler sessionEndHandler

-- | Close the connection to the server.
closeConnection :: XMPP ()
closeConnection = void $ withConnection xmppKillConnection
