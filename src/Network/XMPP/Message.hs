{-# LANGUAGE RecordWildCards #-}
-- | Message handling
module Network.XMPP.Message
       ( Message(..)
       , MessageType(..)
       , MessageError(..)
       , message
       , answerMessage
       )
       where

import Data.Text(Text)
import Data.XML.Types

import Network.XMPP.Types

-- The empty message
message :: Message
message = Message { messageID      = Nothing
                  , messageFrom    = Nothing
                  , messageTo      = Nothing
                  , messageLangTag = Nothing
                  , messageType    = Normal
                  , messagePayload = []
                  }


---- | Create simple message, containing nothing but a body text
--simpleMessage :: JID   -- ^ Recipient
--              -> Text  -- ^ Myssage body
--              -> Message
--simpleMessage to txt = message { messageTo = Just to
--                               , messageBody = Just txt
--                               }

answerMessage  :: Message -> Text -> [Element] -> Maybe Message
answerMessage Message{messageFrom = Just frm, ..} txt payload =
   Just $ Message{ messageFrom    = messageTo
                 , messageID      = Nothing
                 , messageTo      = Just frm
                 , messagePayload = payload
                 , ..
                 }
answerMessage _ _ _ = Nothing

