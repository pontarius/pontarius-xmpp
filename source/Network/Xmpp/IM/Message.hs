{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_HADDOCK hide #-}

module Network.Xmpp.IM.Message where

import Data.Default
import Data.Function
import Data.List
import Data.Text (Text)
import Data.XML.Pickle
import Data.XML.Types
import Network.Xmpp.Marshal
import Network.Xmpp.Types

data MessageBody = MessageBody { bodyLang    :: Maybe LangTag
                               , bodyContent :: Text
                               }

data MessageThread = MessageThread { threadID     :: Text
                                   , threadParent :: Maybe Text
                                   }

data MessageSubject = MessageSubject { subjectLang    :: Maybe LangTag
                                     , subjectContent :: Text
                                     }

-- | The instant message (IM) specific part of a message.
data InstantMessage = InstantMessage { imThread  :: Maybe MessageThread
                                     , imSubject :: [MessageSubject]
                                     , imBody    :: [MessageBody]
                                     }

-- | Empty instant message.
instantMessage :: InstantMessage
instantMessage = InstantMessage { imThread  = Nothing
                                , imSubject = []
                                , imBody    = []
                                }

instance Default InstantMessage where
    def = instantMessage

-- | Get the IM specific parts of a message. Returns 'Nothing' when the received
-- payload is not valid IM data.
getIM :: Message -> Maybe InstantMessage
getIM im = either (const Nothing) Just . unpickle xpIM $ messagePayload im

sanitizeIM :: InstantMessage -> InstantMessage
sanitizeIM im = im{imBody = nubBy ((==) `on` bodyLang) $ imBody im}

-- | Append IM data to a message. Additional IM bodies with the same Langtag are
-- discarded.
withIM :: Message -> InstantMessage -> Message
withIM m im = m{ messagePayload = messagePayload m
                                 ++ pickleTree xpIM (sanitizeIM im) }

imToElements :: InstantMessage -> [Element]
imToElements im = pickle xpIM (sanitizeIM im)

-- | Generate a simple message
simpleIM :: Jid -- ^ recipient
         -> Text -- ^ body
         -> Message
simpleIM to bd = withIM message{messageTo = Just to}
                       instantMessage{imBody = [MessageBody Nothing bd]}

-- | Generate an answer from a received message. The recepient is
-- taken from the original sender, the sender is set to 'Nothing',
-- message ID, language tag, message type as well as subject and
-- thread are inherited.
--
-- Additional IM bodies with the same Langtag are discarded.
answerIM :: [MessageBody] -> Message -> Maybe Message
answerIM bd msg = case getIM msg of
    Nothing -> Nothing
    Just im -> Just $ flip withIM (im{imBody = bd}) $
        message { messageID      = messageID msg
                , messageFrom    = Nothing
                , messageTo      = messageFrom msg
                , messageLangTag = messageLangTag msg
                , messageType    = messageType msg
                }

--------------------------
-- Picklers --------------
--------------------------
xpIM :: PU [Element] InstantMessage
xpIM = xpWrap (\(t, s, b) -> InstantMessage t s b)
              (\(InstantMessage t s b) -> (t, s, b))
       . xpClean
       $ xp3Tuple
           xpMessageThread
           xpMessageSubject
           xpMessageBody


xpMessageSubject :: PU [Element] [MessageSubject]
xpMessageSubject = xpUnliftElems .
                   xpWrap (map $ \(l, s) -> MessageSubject l s)
                          (map $ \(MessageSubject l s) -> (l,s))
                   $ xpElems "{jabber:client}subject" xpLangTag $ xpContent xpId

xpMessageBody :: PU [Element] [MessageBody]
xpMessageBody = xpUnliftElems .
                xpWrap (map $ \(l, s) ->  MessageBody l s)
                       (map $ \(MessageBody l s) -> (l,s))
                   $ xpElems "{jabber:client}body" xpLangTag $ xpContent xpId

xpMessageThread :: PU [Element] (Maybe MessageThread)
xpMessageThread = xpUnliftElems
                  . xpOption
                  . xpWrap (\(t, p) ->  MessageThread p t)
                          (\(MessageThread p t) -> (t,p))
                   $ xpElem "{jabber:client}thread"
                      (xpAttrImplied "parent" xpId)
                      (xpContent xpId)
