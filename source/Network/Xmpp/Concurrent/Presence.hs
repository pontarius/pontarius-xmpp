{-# OPTIONS_HADDOCK hide #-}
module Network.Xmpp.Concurrent.Presence where

import Control.Applicative ((<$>))
import Control.Concurrent.STM
import Lens.Family2 hiding (to)
import Lens.Family2.Stock
import Network.Xmpp.Concurrent.Basic
import Network.Xmpp.Concurrent.Types
import Network.Xmpp.Lens
import Network.Xmpp.Types

-- | Read a presence stanza from the inbound stanza channel, discards any other
-- stanzas. Returns the presence stanza with annotations.
pullPresenceA :: Session -> IO (Either (Annotated PresenceError)
                                      (Annotated Presence))
pullPresenceA session = do
    (stanza, as) <- atomically . readTChan $ stanzaCh session
    case stanza of
        PresenceS p -> return $ Right (p, as)
        PresenceErrorS e -> return $ Left (e, as)
        _ -> pullPresenceA session

-- | Read a presence stanza from the inbound stanza channel, discards any other
-- stanzas. Returns the presence stanza.
pullPresence :: Session -> IO (Either PresenceError Presence)
pullPresence s = either (Left . fst) (Right . fst) <$> pullPresenceA s

-- | Draw and discard stanzas from the inbound channel until a presence stanza matching the given predicate is found. Return the presence stanza with annotations.
waitForPresenceA :: (Annotated Presence -> Bool)
                -> Session
                -> IO (Annotated Presence)
waitForPresenceA f session = do
    s <- pullPresenceA session
    case s of
        Left _ -> waitForPresenceA f session
        Right m | f m -> return m
                | otherwise -> waitForPresenceA f session

-- | Draw and discard stanzas from the inbound channel until a presence stanza matching the given predicate is found. Return the presence stanza with annotations.
waitForPresence :: (Presence -> Bool) -> Session -> IO Presence
waitForPresence f s = fst <$> waitForPresenceA (f . fst) s

-- | Send a presence stanza.
sendPresence :: Presence -> Session -> IO (Either XmppFailure ())
sendPresence p session = sendStanza (PresenceS checkedP) session
  where
    -- | RFC 6121 §3.1.1: When a user sends a presence subscription request to a
    -- potential instant messaging and presence contact, the value of the 'to'
    -- attribute MUST be a bare JID rather than a full JID
    checkedP = case presenceType p of
        Subscribe -> p & to . some_ %~ toBare
        _ -> p
