module Network.Xmpp.Concurrent.Channels.Presence where

import Network.Xmpp.Concurrent.Channels.Types
import Control.Concurrent.STM
import Data.IORef
import Network.Xmpp.Types
import Network.Xmpp.Concurrent.Types
import Network.Xmpp.Concurrent.Channels.Basic

-- | Analogous to 'getMessageChan'.
getPresenceChan :: CSession -> IO (TChan (Either PresenceError Presence))
getPresenceChan session = do
    pCh <- readIORef $ (presenceRef session)
    case pCh of
        Nothing -> do
            pCh' <- atomically $ dupTChan (pShadow session)
            writeIORef (presenceRef session) (Just pCh')
            return pCh'
        Just pCh' -> return pCh'


-- | Analogous to 'dropMessageChan'.
dropPresenceChan :: CSession -> IO ()
dropPresenceChan session = writeIORef (presenceRef session) Nothing


-- | Read an element from the inbound stanza channel, acquiring a copy of the
-- channel as necessary.
pullPresence :: CSession -> IO (Either PresenceError Presence)
pullPresence session = do
    c <- getPresenceChan session
    atomically $ readTChan c

-- | Pulls a (non-error) presence and returns it if the given predicate returns
-- @True@.
waitForPresence :: (Presence -> Bool) -> CSession -> IO Presence
waitForPresence f session = do
    s <- pullPresence session
    case s of
        Left _ -> waitForPresence f session
        Right m | f m -> return m
                | otherwise -> waitForPresence f session

-- | Send a presence stanza.
sendPresence :: Presence -> CSession -> IO ()
sendPresence p session = sendStanza (PresenceS p) session
