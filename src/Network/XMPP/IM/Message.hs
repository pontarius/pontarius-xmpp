{-# LANGUAGE OverloadedStrings #-}
module Network.XMPP.IM.Message
     where

import Data.Text (Text)
import Data.XML.Pickle
import Data.XML.Types

import Network.XMPP.Types
import Network.XMPP.Pickle

data MessageBody = MessageBody (Maybe LangTag) Text
data MessageThread = MessageThread
                        Text          -- ^ Thread ID
                        (Maybe Text)  -- ^ Parent Thread
data MessageSubject = MessageSubject (Maybe LangTag) Text

xpMessageSubject :: PU [Node] MessageSubject
xpMessageSubject = xpWrap (\(l, s) ->  MessageSubject l s)
                          (\(MessageSubject l s) -> (l,s))
                   $ xpElem "{jabber:client}subject" xpLangTag $ xpContent xpId

xpMessageBody :: PU [Node] MessageBody
xpMessageBody = xpWrap (\(l, s) ->  MessageBody l s)
                          (\(MessageBody l s) -> (l,s))
                   $ xpElem "{jabber:client}body" xpLangTag $ xpContent xpId

xpMessageThread :: PU [Node] MessageThread
xpMessageThread = xpWrap (\(t, p) ->  MessageThread p t)
                          (\(MessageThread p t) -> (t,p))
                   $ xpElem "{jabber:client}thread"
                      (xpAttrImplied "parent" xpId)
                      (xpContent xpId)

-- | Get the subject elements of a message (if any). Messages may
-- contain multiple subjects if each of them has a distinct xml:lang
-- attribute
subject :: Message -> [MessageSubject]
subject m = ms
  where
    -- xpFindMatches will _always_ return Right
    Right ms = unpickle (xpFindMatches xpMessageSubject) $ messagePayload m

-- | Get the thread elements of a message (if any). The threads is not considered human readable and no semantic mea
thread :: Message -> Maybe MessageThread
thread m = ms
  where
    -- xpFindMatches will _always_ return Right
    Right ms = unpickle (xpOption xpMessageThread) $ messagePayload m

-- | Get the body elements of a message (if any). Messages may contain
-- multiple bodies if each of them has a distinct xml:lang attribute
body :: Message -> [MessageBody]
body m = ms
  where
    -- xpFindMatches will _always_ return Right
    Right ms = unpickle (xpFindMatches xpMessageBody) $ messagePayload m

newIM
  :: JID
     -> Maybe StanzaId
     -> Maybe LangTag
     -> MessageType
     -> MessageSubject
     -> MessageThread
     -> MessageBody
     -> Message
newIM t i lang tp sbj thrd bdy = Message
    { messageID      = i
    , messageFrom    = Nothing
    , messageTo      = Just t
    , messageLangTag = lang
    , messageType    = tp
    , messagePayload =  pickle xpMessageSubject sbj
                        ++ pickle xpMessageThread thrd
                        ++ pickle xpMessageBody bdy
    }
