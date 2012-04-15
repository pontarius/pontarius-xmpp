{-# LANGUAGE RecordWildCards #-}
module Network.XMPP.Message where

import Data.Text(Text)
import Data.XML.Types

import Network.XMPP.Types

message :: Message
message = Message { messageID      = Nothing
                  , messageFrom    = Nothing
                  , messageTo      = Nothing
                  , messageLangTag = Nothing
                  , messageType    = Normal
                  , messageSubject = Nothing
                  , messageThread  = Nothing
                  , messageBody    = Nothing
                  , messagePayload = []
                  }

simpleMessage :: JID -> Text -> Message
simpleMessage to txt = message { messageTo = Just to
                               , messageBody = Just txt
                               }

answerMessage  :: Message -> Text -> [Element] -> Maybe Message
answerMessage Message{messageFrom = Just frm, ..} txt payload =
   Just $ Message{ messageFrom    = messageTo
                 , messageID      = Nothing
                 , messageTo      = Just frm
                 , messageBody    = Just txt
                 , messagePayload = payload
                 , ..
                 }
answerMessage _ _ _ = Nothing

