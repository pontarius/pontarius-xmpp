{-# LANGUAGE RecordWildCards #-}

{-# OPTIONS_HADDOCK hide #-}

-- | Stanza related functions and constants
--

module Network.Xmpp.Stanza where

import Data.XML.Types
import Network.Xmpp.Types

-- | Request subscription with an entity.
presenceSubscribe :: Jid -> Presence
presenceSubscribe to = presence { presenceTo = Just to
                                , presenceType = Subscribe
                                }

-- | Approve a subscripton of an entity.
presenceSubscribed :: Jid -> Presence
presenceSubscribed to = presence { presenceTo = Just to
                                 , presenceType = Subscribed
                                 }

-- | End a subscription with an entity.
presenceUnsubscribe :: Jid -> Presence
presenceUnsubscribe to = presence { presenceTo = Just to
                                  , presenceType = Unsubscribed
                                  }

-- | Signal to the server that the client is available for communication.
presenceOnline :: Presence
presenceOnline = presence

-- | Signal to the server that the client is no longer available for
-- communication.
presenceOffline :: Presence
presenceOffline = presence {presenceType = Unavailable}

-- | Produce an answer message with the given payload, setting "from" to the
-- "to" attributes in the original message. Produces a 'Nothing' value of the
-- provided message message has no "from" attribute. Sets the "from" attribute
-- to 'Nothing' to let the server assign one.
answerMessage :: Message -> [Element] -> Maybe Message
answerMessage Message{messageFrom = Just frm, ..} payload =
    Just Message{ messageFrom    = Nothing
                , messageID      = Nothing
                , messageTo      = Just frm
                , messagePayload = payload
                , ..
                }
answerMessage _ _ = Nothing

-- | Add a recipient to a presence notification.
presTo :: Presence -> Jid -> Presence
presTo pres to = pres{presenceTo = Just to}
