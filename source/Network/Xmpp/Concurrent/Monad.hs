{-# LANGUAGE OverloadedStrings #-}
module Network.Xmpp.Concurrent.Monad where

import           Network.Xmpp.Types

import           Control.Applicative((<$>))
import           Control.Concurrent
import           Control.Concurrent.STM
import qualified Control.Exception.Lifted as Ex
import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Control.Monad.State.Strict

import           Data.IORef
import qualified Data.Map as Map
import           Data.Text(Text)

import           Network.Xmpp.Concurrent.Types
import           Network.Xmpp.Monad


-- | Retrieves an IQ listener channel. If the namespace/'IQRequestType' is not
-- already handled, a new 'TChan' is created and returned as a 'Right' value.
-- Otherwise, the already existing channel will be returned wrapped in a 'Left'
-- value. Note that the 'Left' channel might need to be duplicated in order not
-- to interfere with existing consumers.
listenIQChan :: IQRequestType  -- ^ Type of IQs to receive (@Get@ or @Set@)
             -> Text -- ^ Namespace of the child element
             -> Chans
             -> IO (Either (TChan IQRequestTicket) (TChan IQRequestTicket))
listenIQChan tp ns chans = do
    let handlers = iqHandlers chans
    atomically $ do
        (byNS, byID) <- readTVar handlers
        iqCh <- newTChan
        let (present, byNS') = Map.insertLookupWithKey'
                (\_ _ old -> old)
                (tp, ns)
                iqCh
                byNS
        writeTVar handlers (byNS', byID)
        return $ case present of
            Nothing -> Right iqCh
            Just iqCh' -> Left iqCh'

-- | Get a duplicate of the stanza channel
getStanzaChan :: Chans -> IO (TChan Stanza)
getStanzaChan chans = atomically $ dupTChan (sShadow chans)

-- | Get the inbound stanza channel, duplicates from master if necessary. Please
-- note that once duplicated it will keep filling up, call 'dropMessageChan' to
-- allow it to be garbage collected.
getMessageChan :: Chans -> IO (TChan (Either MessageError Message))
getMessageChan chans = do
    mCh <- readIORef $ messagesRef chans
    case mCh of
        Nothing -> do
            mCh' <- atomically $ dupTChan (mShadow chans)
            writeIORef (messagesRef chans) (Just mCh')
            return mCh'
        Just mCh' -> return mCh'

-- | Analogous to 'getMessageChan'.
getPresenceChan :: Chans -> IO (TChan (Either PresenceError Presence))
getPresenceChan chans = do
    pCh <- readIORef $ presenceRef chans
    case pCh of
        Nothing -> do
            pCh' <- atomically $ dupTChan (pShadow chans)
            writeIORef (presenceRef chans) (Just pCh')
            return pCh'
        Just pCh' -> return pCh'

-- | Drop the local end of the inbound stanza channel from our context so it can
-- be GC-ed.
dropMessageChan :: Chans -> IO ()
dropMessageChan chans = writeIORef (messagesRef chans) Nothing

-- | Analogous to 'dropMessageChan'.
dropPresenceChan :: Chans -> IO ()
dropPresenceChan chans = writeIORef (presenceRef chans) Nothing

-- | Read an element from the inbound stanza channel, acquiring a copy of the
-- channel as necessary.
pullMessage :: Chans -> IO (Either MessageError Message)
pullMessage chans = do
    c <- getMessageChan chans
    atomically $ readTChan c

-- | Read an element from the inbound stanza channel, acquiring a copy of the
-- channel as necessary.
pullPresence :: Chans -> IO (Either PresenceError Presence)
pullPresence chans = do
    c <- getPresenceChan chans
    atomically $ readTChan c

-- | Send a stanza to the server.
sendStanza :: Stanza -> Chans -> IO ()
sendStanza a chans = atomically $ writeTChan (outCh chans) a


-- | Create a forked chans object
forkChans :: Chans -> IO Chans
forkChans chans = do
    mCH' <- newIORef Nothing
    pCH' <- newIORef Nothing
    return $ chans {messagesRef = mCH', presenceRef = pCH'}

-- | Pulls a message and returns it if the given predicate returns @True@.
filterMessages :: (MessageError -> Bool)
               -> (Message -> Bool)
               -> Chans -> IO (Either MessageError Message)
filterMessages f g chans = do
    s <- pullMessage chans
    case s of
        Left  e | f e -> return $ Left e
                | otherwise -> filterMessages f g chans
        Right m | g m -> return $ Right m
                | otherwise -> filterMessages f g chans

-- | Pulls a (non-error) message and returns it if the given predicate returns
-- @True@.
waitForMessage :: (Message -> Bool) -> Chans -> IO Message
waitForMessage f chans = do
    s <- pullMessage chans
    case s of
        Left _ -> waitForMessage f chans
        Right m | f m -> return m
                | otherwise -> waitForMessage f chans


-- | Pulls an error message and returns it if the given predicate returns @True@.
waitForMessageError :: (MessageError -> Bool) -> Chans -> IO MessageError
waitForMessageError f chans = do
    s <- pullMessage chans
    case s of
        Right _ -> waitForMessageError f chans
        Left  m | f m -> return m
                | otherwise -> waitForMessageError f chans


-- | Pulls a (non-error) presence and returns it if the given predicate returns
-- @True@.
waitForPresence :: (Presence -> Bool) -> Chans -> IO Presence
waitForPresence f chans = do
    s <- pullPresence chans
    case s of
        Left _ -> waitForPresence f chans
        Right m | f m -> return m
                | otherwise -> waitForPresence f chans

-- TODO: Wait for presence error?

-- | Run an XmppMonad action in isolation. Reader and writer workers will be
-- temporarily stopped and resumed with the new session details once the action
-- returns. The action will run in the calling thread. Any uncaught exceptions
-- will be interpreted as connection failure.
withConnection :: XmppConMonad a -> Session -> IO (Either StreamError a)
withConnection a session =  do
    wait <- newEmptyTMVarIO
    Ex.mask_ $ do
        -- Suspends the reader until the lock (wait) is released (set to `()').
        throwTo (readerThread session) $ Interrupt wait
        -- We acquire the write and stateRef locks, to make sure that this is
        -- the only thread that can write to the stream and to perform a
        -- withConnection calculation. Afterwards, we release the lock and
        -- fetches an updated state.
        s <- Ex.catch
            (atomically $ do
                 _ <- takeTMVar  (writeRef session)
                 s <- takeTMVar (conStateRef session)
                 putTMVar wait ()
                 return s
            )
            -- If we catch an exception, we have failed to take the MVars above.
            (\e -> atomically (putTMVar wait ()) >>
                 Ex.throwIO (e :: Ex.SomeException)
            )
        -- Run the XmppMonad action, save the (possibly updated) states, release
        -- the locks, and return the result.
        Ex.catches
            (do
                 (res, s') <- runStateT a s
                 atomically $ do
                     putTMVar (writeRef session) (sConPushBS s')
                     putTMVar (conStateRef session) s'
                     return $ Right res
            )
            -- We treat all Exceptions as fatal. If we catch a StreamError, we
            -- return it. Otherwise, we throw an exception.
            [ Ex.Handler $ \e -> return $ Left (e :: StreamError)
            , Ex.Handler $ \e -> runStateT xmppKillConnection s
                  >> Ex.throwIO (e :: Ex.SomeException)
            ]

-- | Send a presence stanza.
sendPresence :: Presence -> Chans -> IO ()
sendPresence p chans = sendStanza (PresenceS p) chans

-- | Send a message stanza.
sendMessage :: Message -> Chans -> IO ()
sendMessage m chans = sendStanza (MessageS m) chans


-- | Executes a function to update the event handlers.
modifyHandlers :: (EventHandlers -> EventHandlers) -> Session -> IO ()
modifyHandlers f session = atomically $ modifyTVar (eventHandlers session) f

-- | Sets the handler to be executed when the server connection is closed.
setConnectionClosedHandler :: (StreamError -> Session -> IO ()) -> Session -> IO ()
setConnectionClosedHandler eh session = do
    modifyHandlers (\s -> s{connectionClosedHandler =
                                 \e -> eh e session}) session

-- | Run an event handler.
runHandler :: (EventHandlers -> IO a) -> Session -> IO a
runHandler h session = h =<< atomically (readTVar $ eventHandlers session)


-- | End the current Xmpp session.
endSession :: Session -> IO ()
endSession session =  do -- TODO: This has to be idempotent (is it?)
    void $ withConnection xmppKillConnection session
    stopThreads session

-- | Close the connection to the server. Closes the stream (by enforcing a
-- write lock and sending a </stream:stream> element), waits (blocks) for three
-- seconds, and then closes the connection.
closeConnection :: Session -> IO ()
closeConnection session = Ex.mask_ $ do
    send <- atomically $ takeTMVar (writeRef session)
    cc <- sCloseConnection <$> ( atomically $ readTMVar (conStateRef session))
    send "</stream:stream>"
    void . forkIO $ do
      threadDelay 3000000
      -- When we close the connection, we close the handle that was used in the
      -- sCloseConnection above. So even if a new connection has been
      -- established at this point, it will not be affected by this action.
      (Ex.try cc) :: IO (Either Ex.SomeException ())
      return ()
    atomically $ putTMVar (writeRef session) (\_ -> return False)
