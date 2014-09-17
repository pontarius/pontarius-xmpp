module Network.Xmpp.IM.PresenceTracker.Types where

import           Data.Map (Map)

import           Network.Xmpp.Types
import           Network.Xmpp.IM.Presence

-- Map from bare JIDs to a map of full JIDs to show maybe status.
--
-- Invariants:
-- * The outer map should not have entries for bare JIDs that have no
--   available resource, i.e. the inner map should never be empty
--
-- * The inner map keys' local and domain part coincide with the outer keys'
newtype Peers = Peers { unPeers :: Map Jid (Map Jid (Maybe IMPresence))}
                deriving (Show)

data PeerStatus = PeerAvailable (Maybe IMPresence)
                | PeerUnavailable
                  deriving (Show, Eq)
