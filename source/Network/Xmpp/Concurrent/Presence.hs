{-# OPTIONS_HADDOCK hide #-}
module Network.Xmpp.Concurrent.Presence where

import Control.Concurrent.STM
import Data.IORef
import Network.Xmpp.Types
import Network.Xmpp.Concurrent.Types
import Network.Xmpp.Concurrent.Basic

-- | Read an element from the inbound stanza channel, acquiring a copy of the
-- channel as necessary.
pullPresence :: Session -> IO (Either PresenceError Presence)
pullPresence session = do
    stanza <- atomically . readTChan $ stanzaCh session
    case stanza of
        PresenceS p -> return $ Right p
        PresenceErrorS e -> return $ Left e
        _ -> pullPresence session

-- | Pulls a (non-error) presence and returns it if the given predicate returns
-- @True@.
waitForPresence :: (Presence -> Bool) -> Session -> IO Presence
waitForPresence f session = do
    s <- pullPresence session
    case s of
        Left _ -> waitForPresence f session
        Right m | f m -> return m
                | otherwise -> waitForPresence f session

-- | Send a presence stanza.
sendPresence :: Presence -> Session -> IO ()
sendPresence p session = sendStanza (PresenceS p) session
