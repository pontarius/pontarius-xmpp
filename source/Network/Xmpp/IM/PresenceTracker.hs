{-# LANGUAGE RankNTypes #-}
module Network.Xmpp.IM.PresenceTracker where

import           Control.Applicative
import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Monad
import qualified Data.Foldable as Foldable
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe
import           Lens.Family2 hiding (Prism)
import           Lens.Family2.Stock hiding (from)
import           Network.Xmpp.Concurrent.Types
import           Network.Xmpp.IM.Presence
import           Network.Xmpp.Lens hiding (Lens, Traversal)
import           Network.Xmpp.Types
import           Prelude hiding (mapM)

import           Network.Xmpp.IM.PresenceTracker.Types

_peers :: Iso Peers (Map Jid (Map Jid (Maybe IMPresence)))
_peers = mkIso unPeers Peers

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


-- | Status of give full JID
peerStatusL :: Jid -> Lens' Peers PeerStatus
peerStatusL j = _peers . at (toBare j)  . maybeMap . at j . _PeerStatus

peerMapPeerAvailable :: Jid -> Peers -> Bool
peerMapPeerAvailable j | isFull j = not . nullOf (peerStatusL j . _PeerAvailable)
                       | otherwise = not . nullOf (_peers . at j . some_)

handlePresence :: Maybe (Jid -> PeerStatus -> PeerStatus -> IO ())
               -> TVar Peers
               -> StanzaHandler
handlePresence onChange peers _ st _  = do
        let mbPr = do
                pr <- st ^? _Stanza . _Presence -- Only act on presence stanzas
                fr <- pr ^? from . some_ . _isFull -- Only act on full JIDs
                return (pr, fr)
        Foldable.forM_ mbPr $ \(pr, fr) ->
            case presenceType pr of
                Available -> setStatus fr   (PeerAvailable (getIMPresence pr))
                Unavailable -> setStatus fr PeerUnavailable
                _ -> return ()
        return [(st, [])]
  where
    setStatus fr newStatus = do
        os <- atomically $ do
            ps <- readTVar peers
            let oldStatus = ps ^. peerStatusL fr
            writeTVar peers $ ps & set (peerStatusL fr) newStatus
            return oldStatus
        unless (os == newStatus) $ case onChange of
            Nothing -> return ()
            Just oc -> void . forkIO $ oc fr os newStatus
        return ()

-- | Check whether a given jid is available
isPeerAvailable :: Jid -> Session -> STM Bool
isPeerAvailable j sess = peerMapPeerAvailable j <$> readTVar (presenceRef sess)

-- | Get status of given full JID
getEntityStatus :: Jid -> Session -> STM PeerStatus
getEntityStatus j sess = do
    peers <- readTVar (presenceRef sess)
    return $ peers ^. peerStatusL j

-- | Get list of (bare) Jids with available entities
getAvailablePeers :: Session -> STM [Jid]
getAvailablePeers sess = do
    Peers peers <- readTVar (presenceRef sess)
    return $ Map.keys peers

-- | Get all available full JIDs to the given JID
getPeerEntities :: Jid -> Session -> STM (Map Jid (Maybe IMPresence))
getPeerEntities j sess = do
    Peers peers <- readTVar (presenceRef sess)
    case Map.lookup (toBare j) peers of
        Nothing -> return Map.empty
        Just js -> return js
