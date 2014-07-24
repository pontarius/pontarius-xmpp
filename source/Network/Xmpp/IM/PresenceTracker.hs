{-# LANGUAGE RankNTypes #-}
module Network.Xmpp.IM.PresenceTracker where

import           Control.Applicative
import           Control.Concurrent.STM
import           Control.Monad (guard)
import           Data.Foldable
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe
import           Data.Traversable
import           Lens.Family2
import           Lens.Family2.Stock
import           Network.Xmpp.Concurrent.Types
import           Network.Xmpp.IM.Presence
import           Network.Xmpp.IM.Roster
import           Network.Xmpp.Lens hiding (Lens, Traversal)
import           Network.Xmpp.Types
import           Prelude hiding (mapM)

-- Map from bare JIDs to a map of full JIDs to show maybe status.
--
-- Invariants:
-- * The outer map should not have entries for bare JIDs that have no
--   available resource, i.e. the inner map should never be empty
--
-- * The inner map keys' local and domain part coincide with the outer keys'
newtype Peers = Peers { unPeers :: Map Jid (Map Jid (Maybe IMPresence))}
                deriving (Show)

_peers :: Iso Peers (Map Jid (Map Jid (Maybe IMPresence)))
_peers = mkIso unPeers Peers

data PeerStatus = PeerAvailable (Maybe IMPresence)
                | PeerUnavailable
                  deriving (Show)

_PeerAvailable :: Prism PeerStatus (Maybe IMPresence)
_PeerAvailable = prism' PeerAvailable fromPeerAvailable
  where
    fromPeerAvailable (PeerAvailable pa) = Just pa
    fromPeerAvailable _  = Nothing

_PeerUnavailable :: Prism PeerStatus ()
_PeerUnavailable = prism' (const PeerUnavailable) fromPeerUnavailable
  where
    fromPeerUnavailable PeerUnavailable = Just ()
    fromPeerUnavailable _ = Nothing

_PeerStatus :: Iso (Maybe (Maybe IMPresence)) PeerStatus
_PeerStatus = mkIso toPeerStatus fromPeerStatus
  where
    toPeerStatus (Nothing) = PeerUnavailable
    toPeerStatus (Just imp) = PeerAvailable imp
    fromPeerStatus PeerUnavailable = Nothing
    fromPeerStatus (PeerAvailable imp) = Just imp

maybeMap :: Iso (Maybe (Map a b)) (Map a b)
maybeMap = mkIso maybeToMap mapToMaybe
  where
    maybeToMap Nothing = Map.empty
    maybeToMap (Just m) = m
    mapToMaybe m | Map.null m = Nothing
                 | otherwise = Just m

peerStatusL :: Jid -> Lens' Peers PeerStatus
peerStatusL j = _peers . at (toBare j)  . maybeMap . at j . _PeerStatus

peerMapPeerAvailable :: Jid -> Peers -> Bool
peerMapPeerAvailable j =  not . nullOf (peerStatusL j . _PeerAvailable)

statusTracker :: (Stanza -> IO (Either XmppFailure ()))
              -> IO (Plugin', TVar Peers)
statusTracker out = do
    sessRef <- newTVarIO Nothing
    peerMap <- newTVarIO (Peers Map.empty)

    return (Plugin' { inHandler = handleIn sessRef peerMap
                    , outHandler = out
                    , onSessionUp = atomically . writeTVar sessRef . Just
                    }, peerMap)
  where
    handleIn sessRef peers st _ = do
        mbRoster <- atomically (mapM getRoster' =<< readTVar sessRef)
        let mbPr = do
                pr <- st ^? _Presence -- Only act on presence stanzas
                fr <- pr ^? from . _Just . _isFull -- Only act on full JIDs
                roster <- mbRoster
                -- Check that the from address is in our roster. This means that
                -- deactivating the roster effectively turns off this plugin
                guard $ Map.member (toBare fr) (roster ^. itemsL)
                return (pr, fr)
        forM_ mbPr $ \(pr, fr) ->
            case presenceType pr of
                Available -> atomically . modifyTVar peers
                                 $ set (peerStatusL fr)
                                       (PeerAvailable (getIMPresence pr))
                Unavailable -> atomically . modifyTVar peers
                                 $ set (peerStatusL fr) PeerUnavailable
                _ -> return ()
        return [(st, [])]

isPeerAvailable :: Jid -> TVar Peers -> STM Bool
isPeerAvailable j peerRef = peerMapPeerAvailable j <$> readTVar peerRef
