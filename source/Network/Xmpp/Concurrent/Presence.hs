{-# OPTIONS_HADDOCK hide #-}
module Network.Xmpp.Concurrent.Presence where

import Control.Concurrent.STM
import Network.Xmpp.Types
import Network.Xmpp.Concurrent.Types
import Network.Xmpp.Concurrent.Basic

-- | Read an element from the inbound stanza channel, discardes any non-Presence
-- stanzas from the channel
pullPresence :: Session -> IO (Either (Annotated PresenceError)
                                      (Annotated Presence))
pullPresence session = do
    (stanza, as) <- atomically . readTChan $ stanzaCh session
    case stanza of
        PresenceS p -> return $ Right (p, as)
        PresenceErrorS e -> return $ Left (e, as)
        _ -> pullPresence session

-- | Pulls a (non-error) presence and returns it if the given predicate returns
-- @True@.
waitForPresence :: (Annotated Presence -> Bool)
                -> Session
                -> IO (Annotated Presence)
waitForPresence f session = do
    s <- pullPresence session
    case s of
        Left _ -> waitForPresence f session
        Right m | f m -> return m
                | otherwise -> waitForPresence f session

-- | Send a presence stanza.
sendPresence :: Presence -> Session -> IO Bool
sendPresence p session = sendStanza (PresenceS p) session
