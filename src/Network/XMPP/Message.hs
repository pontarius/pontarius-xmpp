{-# LANGUAGE RecordWildCards #-}

module Network.XMPP.Message
    ( Message(..)
    , MessageError(..)
    , MessageType(..)
    , answerMessage
    , message
    ) where

import Data.Text (Text)
import Data.XML.Types

import Network.XMPP.Types

-- | An empty message.
message :: Message
message = Message { messageID      = Nothing
                  , messageFrom    = Nothing
                  , messageTo      = Nothing
                  , messageLangTag = Nothing
                  , messageType    = Normal
                  , messagePayload = []
                  }

-- Produce an answer message with the given payload, switching the "from" and
-- "to" attributes in the original message.
answerMessage :: Message -> [Element] -> Maybe Message
answerMessage Message{messageFrom = Just frm, ..} payload =
    Just Message{ messageFrom    = messageTo
                , messageID      = Nothing
                , messageTo      = Just frm
                , messagePayload = payload
                , ..
                }
answerMessage _ _ = Nothing