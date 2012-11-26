module Network.Xmpp.Concurrent.Channels.Message where

import Network.Xmpp.Concurrent.Channels.Types
import Control.Concurrent.STM
import Data.IORef
import Network.Xmpp.Types
import Network.Xmpp.Concurrent.Types
import Network.Xmpp.Concurrent.Channels.Basic

-- | Get the inbound stanza channel, duplicates from master if necessary. Please
-- note that once duplicated it will keep filling up, call 'dropMessageChan' to
-- allow it to be garbage collected.
getMessageChan :: CSession -> IO (TChan (Either MessageError Message))
getMessageChan session = do
    mCh <- readIORef . messagesRef $ session
    case mCh of
        Nothing -> do
            mCh' <- atomically $ dupTChan (mShadow session)
            writeIORef (messagesRef session) (Just mCh')
            return mCh'
        Just mCh' -> return mCh'

-- | Drop the local end of the inbound stanza channel from our context so it can
-- be GC-ed.
dropMessageChan :: CSession -> IO ()
dropMessageChan session = writeIORef (messagesRef session) Nothing

-- | Read an element from the inbound stanza channel, acquiring a copy of the
-- channel as necessary.
pullMessage :: CSession -> IO (Either MessageError Message)
pullMessage session = do
    c <- getMessageChan session
    atomically $ readTChan c

-- | Pulls a (non-error) message and returns it if the given predicate returns
-- @True@.
waitForMessage :: (Message -> Bool) -> CSession -> IO Message
waitForMessage f session = do
    s <- pullMessage session
    case s of
        Left _ -> waitForMessage f session
        Right m | f m -> return m
                | otherwise -> waitForMessage f session

-- | Pulls an error message and returns it if the given predicate returns @True@.
waitForMessageError :: (MessageError -> Bool) -> CSession -> IO MessageError
waitForMessageError f session = do
    s <- pullMessage session
    case s of
        Right _ -> waitForMessageError f session
        Left  m | f m -> return m
                | otherwise -> waitForMessageError f session


-- | Pulls a message and returns it if the given predicate returns @True@.
filterMessages :: (MessageError -> Bool)
               -> (Message -> Bool)
               -> CSession -> IO (Either MessageError Message)
filterMessages f g session = do
    s <- pullMessage session
    case s of
        Left  e | f e -> return $ Left e
                | otherwise -> filterMessages f g session
        Right m | g m -> return $ Right m
                | otherwise -> filterMessages f g session

-- | Send a message stanza.
sendMessage :: Message -> CSession -> IO ()
sendMessage m session = sendStanza (MessageS m) session
