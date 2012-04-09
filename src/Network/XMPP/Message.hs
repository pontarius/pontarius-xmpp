module Network.XMPP.Message where

import Data.Text(Text)
import Data.XML.Types

import Network.XMPP.Types

simpleMessage :: JID -> Text -> Message
simpleMessage to txt =
    Message Nothing to Nothing Nothing Nothing (Just txt) Nothing []

answerMessage  :: Message -> Text -> [Element] -> Maybe Message
answerMessage (Message (Just frm) _to id tp subject _txt thread _ext) txt bodies =
    Just $ Message Nothing frm id tp subject (Just txt) thread bodies
answerMessage _ _ _ = Nothing