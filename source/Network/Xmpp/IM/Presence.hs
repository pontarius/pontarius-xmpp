{-# OPTIONS_HADDOCK hide #-}

module Network.Xmpp.IM.Presence where

import Data.Text(Text)
import Network.Xmpp.Types

-- | An empty presence.
presence :: Presence
presence = Presence { presenceID       = Nothing
                    , presenceFrom     = Nothing
                    , presenceTo       = Nothing
                    , presenceLangTag  = Nothing
                    , presenceType     = Nothing
                    , presencePayload  = []
                    }

-- | Request subscription with an entity.
presenceSubscribe :: Jid -> Presence
presenceSubscribe to = presence { presenceTo = Just to
                                , presenceType = Just Subscribe
                                }

-- | Is presence a subscription request?
isPresenceSubscribe :: Presence -> Bool
isPresenceSubscribe pres = presenceType pres == (Just Subscribe)

-- | Approve a subscripton of an entity.
presenceSubscribed :: Jid -> Presence
presenceSubscribed to = presence { presenceTo = Just to
                                 , presenceType = Just Subscribed
                                 }

-- | Is presence a subscription approval?
isPresenceSubscribed :: Presence -> Bool
isPresenceSubscribed pres = presenceType pres == (Just Subscribed)

-- | End a subscription with an entity.
presenceUnsubscribe :: Jid -> Presence
presenceUnsubscribe to = presence { presenceTo = Just to
                                  , presenceType = Just Unsubscribed
                                  }

-- | Is presence an unsubscription request?
isPresenceUnsubscribe :: Presence -> Bool
isPresenceUnsubscribe pres = presenceType pres == (Just Unsubscribe)

-- | Signal to the server that the client is available for communication.
presenceOnline :: Presence
presenceOnline = presence

-- | Signal to the server that the client is no longer available for
-- communication.
presenceOffline :: Presence
presenceOffline = presence {presenceType = Just Unavailable}

---- Change your status
--status
--  :: Maybe Text     -- ^ Status message
--  -> Maybe ShowType -- ^ Status Type
--  -> Maybe Int      -- ^ Priority
--  -> Presence
--status txt showType prio = presence { presenceShowType = showType
--                                    , presencePriority = prio
--                                    , presenceStatus   = txt
--                                    }

-- | Set the current availability status. This implicitly sets the client's
-- status online.
--presenceAvail :: ShowType -> Presence
--presenceAvail showType = status Nothing (Just showType) Nothing

-- | Set the current status message. This implicitly sets the client's status
-- online.
--presenceMessage :: Text -> Presence
--presenceMessage txt = status (Just txt) Nothing Nothing