module Network.XMPP.Presence where

import Data.Text(Text)
import Network.XMPP.Types

presenceSubscribe :: JID -> Presence
presenceSubscribe to = Presence Nothing (Just to) Nothing (Just Subscribe) Nothing Nothing Nothing []

-- | Is presence a subscription request
isPresenceSubscribe :: Presence -> Bool
isPresenceSubscribe pres = pType pres == (Just Subscribe)

-- | Approve a subscripton of an entity
presenceSubscribed :: JID -> Presence
presenceSubscribed to = Presence Nothing (Just to) Nothing (Just Subscribed) Nothing Nothing Nothing []

-- | Is presence a subscription approval
isPresenceSubscribed :: Presence -> Bool
isPresenceSubscribed pres = pType pres == (Just Subscribed)

-- | End a subscription with an entity
presenceUnsubscribe :: JID -> Presence
presenceUnsubscribe to = Presence Nothing (Just to) Nothing (Just Unsubscribe) Nothing Nothing Nothing []

-- | Is presence an unsubscription request
isPresenceUnsubscribe :: Presence -> Bool
isPresenceUnsubscribe pres = pType pres == (Just Unsubscribe)

-- | Signals to the server that the client is available for communication
presenceOnline :: Presence
presenceOnline = Presence Nothing Nothing Nothing Nothing Nothing Nothing Nothing []

-- | Signals to the server that the client is no longer available for communication.
presenceOffline :: Presence
presenceOffline = Presence Nothing Nothing Nothing (Just Unavailable) Nothing Nothing Nothing []

presence
  :: Maybe Text     -- ^ Status message
  -> Maybe ShowType -- ^ Status Type
  -> Maybe Int      -- ^ Priority
  -> Presence
presence txt showType priority = Presence Nothing Nothing Nothing Nothing showType txt priority []

-- | Sets the current availability status. This implicitly sets the clients
-- status online
presenceAvail :: ShowType -> Presence
presenceAvail showType = presence Nothing (Just showType) Nothing

-- | Sets the current status message. This implicitly sets the clients
-- status online
presenceMessage :: Text -> Presence
presenceMessage txt = presence (Just txt) Nothing Nothing

-- | Adds a recipient to a presence notification
presenceTo :: Presence -> JID -> Presence
presenceTo pres to = pres{pTo = Just to}