module Network.XMPP.Presence where

import Data.Text(Text)
import Network.XMPP.Types


presence :: Presence
presence = Presence { presenceID       = Nothing
                    , presenceFrom     = Nothing
                    , presenceTo       = Nothing
                    , presenceLangTag  = Nothing
                    , presenceType     = Nothing
                    , presenceShowType = Nothing
                    , presenceStatus   = Nothing
                    , presencePriority = Nothing
                    , presencePayload  = []
                    }

presenceSubscribe :: JID -> Presence
presenceSubscribe to = presence { presenceTo = Just to
                                , presenceType = Just Subscribe
                                }

-- | Is presence a subscription request
isPresenceSubscribe :: Presence -> Bool
isPresenceSubscribe pres = presenceType pres == (Just Subscribe)

-- | Approve a subscripton of an entity
presenceSubscribed :: JID -> Presence
presenceSubscribed to = presence { presenceTo = Just to
                                 , presenceType = Just Subscribed
                                 }

-- | Is presence a subscription approval
isPresenceSubscribed :: Presence -> Bool
isPresenceSubscribed pres = presenceType pres == (Just Subscribed)

-- | End a subscription with an entity
presenceUnsubscribe :: JID -> Presence
presenceUnsubscribe to = presence { presenceTo = Just to
                                  , presenceType = Just Unsubscribed
                                  }

-- | Is presence an unsubscription request
isPresenceUnsubscribe :: Presence -> Bool
isPresenceUnsubscribe pres = presenceType pres == (Just Unsubscribe)

-- | Signals to the server that the client is available for communication
presenceOnline :: Presence
presenceOnline = presence

-- | Signals to the server that the client is no longer available for communication.
presenceOffline :: Presence
presenceOffline = presence {presenceType = Just Unavailable}

status
  :: Maybe Text     -- ^ Status message
  -> Maybe ShowType -- ^ Status Type
  -> Maybe Int      -- ^ Priority
  -> Presence
status txt showType prio = presence { presenceShowType = showType
                                    , presencePriority = prio
                                    , presenceStatus   = txt
                                    }

-- | Sets the current availability status. This implicitly sets the clients
-- status online
presenceAvail :: ShowType -> Presence
presenceAvail showType = status Nothing (Just showType) Nothing

-- | Sets the current status message. This implicitly sets the clients
-- status online
presenceMessage :: Text -> Presence
presenceMessage txt = status (Just txt) Nothing Nothing

-- | Adds a recipient to a presence notification
presTo :: Presence -> JID -> Presence
presTo pres to = pres{presenceTo = Just to}