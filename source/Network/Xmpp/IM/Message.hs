{-# LANGUAGE OverloadedStrings #-}
module Network.Xmpp.IM.Message
     where

import Control.Applicative ((<$>))

import Data.Maybe (maybeToList)
import Data.Text (Text)
import Data.XML.Pickle
import Data.XML.Types

import Network.Xmpp.Types
import Network.Xmpp.Pickle

data MessageBody = MessageBody (Maybe LangTag) Text
data MessageThread = MessageThread
                        Text          -- Thread ID
                        (Maybe Text)  -- Parent Thread
data MessageSubject = MessageSubject (Maybe LangTag) Text

xpMessageSubject :: PU [Element] MessageSubject
xpMessageSubject = xpUnliftElems .
                   xpWrap (\(l, s) ->  MessageSubject l s)
                          (\(MessageSubject l s) -> (l,s))
                   $ xpElem "{jabber:client}subject" xpLangTag $ xpContent xpId

xpMessageBody :: PU [Element] MessageBody
xpMessageBody = xpUnliftElems .
                xpWrap (\(l, s) ->  MessageBody l s)
                          (\(MessageBody l s) -> (l,s))
                   $ xpElem "{jabber:client}body" xpLangTag $ xpContent xpId

xpMessageThread :: PU [Element] MessageThread
xpMessageThread = xpUnliftElems
                  . xpWrap (\(t, p) ->  MessageThread p t)
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

-- | Get the thread elements of a message (if any). The thread of a
-- message is considered opaque, that is, no meaning, other than it's
-- literal identity, may be derived from it and it is not human
-- readable
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

-- | Generate a new instant message
newIM
  :: Jid
     -> Maybe StanzaId
     -> Maybe LangTag
     -> MessageType
     -> Maybe MessageSubject
     -> Maybe MessageThread
     -> Maybe MessageBody
     -> [Element]
     -> Message
newIM t i lang tp sbj thrd bdy payload = Message
    { messageID      = i
    , messageFrom    = Nothing
    , messageTo      = Just t
    , messageLangTag = lang
    , messageType    = tp
    , messagePayload =  concat $
                        maybeToList (pickle xpMessageSubject <$> sbj)
                        ++ maybeToList (pickle xpMessageThread <$> thrd)
                        ++ maybeToList (pickle xpMessageBody <$> bdy)
                        ++ [payload]
    }

-- | Generate a simple instance message
simpleIM :: Jid -> Text -> Message
simpleIM t bd = newIM
                  t
                  Nothing
                  Nothing
                  Normal
                  Nothing
                  Nothing
                  (Just $ MessageBody Nothing bd)
                  []

-- | Generate an answer from a received message. The recepient is
-- taken from the original sender, the sender is set to Nothing,
-- message ID, language tag, message type as well as subject and
-- thread are inherited, the remaining payload is replaced by the
-- given one
answerIM :: Maybe MessageBody -> [Element] -> Message -> Message
answerIM bd payload msg = Message
    { messageID      = messageID msg
    , messageFrom    = Nothing
    , messageTo      = messageFrom msg
    , messageLangTag = messageLangTag msg
    , messageType    = messageType msg
    , messagePayload =  concat $
                        (pickle xpMessageSubject <$> subject msg)
                        ++ maybeToList (pickle xpMessageThread <$> thread msg)
                        ++ maybeToList (pickle xpMessageBody <$> bd)
                        ++ [payload]
    }
