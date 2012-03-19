{-# LANGUAGE OverloadedStrings #-}

module Network.XMPP.Types where
-- proudly "borrowed" from haskell-xmpp

import Control.Applicative((<$>))
import Control.Monad

import Data.Maybe
import Data.Text as Text
import Data.String as Str
import Data.XML.Types

class ToText a where
  toText :: a -> Text

class FromText a where
  fromText :: Text -> a

-- | Jabber ID (JID) datatype
data JID = JID { node :: Maybe Text
               -- ^ Account name
               , domain :: Text
               -- ^ Server adress
               , resource :: Maybe Text
               -- ^ Resource name
               }
instance ToText JID where
  toText (JID n d r) =
    let n' = maybe "" (`append` "@" ) n
        r' = maybe "" ("/" `append` ) r
    in Text.concat [n', d, r']

instance FromText JID where
  fromText = parseJID

instance Show JID where
  show = Text.unpack . toText

-- Ugh, that smells a bit.
parseJID jid =
  let (jid', rst) = case Text.splitOn "@" jid of
                      [rest] -> (JID Nothing, rest)
                      [node,rest] -> (JID (Just node), rest)
                      _ -> error $  "Couldn't parse JID: \"" ++ Text.unpack jid ++ "\""
  in case Text.splitOn "/" rst of
      [domain] -> jid' domain Nothing
      [domain, resource] -> jid' domain (Just resource)
      _ -> error $ "Couldn't parse JID: \"" ++ Text.unpack jid ++ "\""

instance IsString JID where
  fromString = parseJID . Text.pack


-- should we factor from, to and id out, even though they are
-- sometimes mandatory?
data Message = Message
    { mFrom :: Maybe JID
    , mTo :: JID
    , mId :: Maybe Text
             -- ^ Message 'from', 'to', 'id' attributes
    , mType :: MessageType
               -- ^ Message type (2.1.1)
    , mSubject :: Maybe Text
                  -- ^ Subject element (2.1.2.1)
    , mBody :: Maybe Text
               -- ^ Body element (2.1.2.2)
    , mThread :: Maybe Text
                 -- ^ Thread element (2.1.2.3)
    , mExt :: [Element]
              -- ^ Additional contents, used for extensions
    } deriving Show

data Presence =  Presence
    { pFrom :: Maybe JID
    , pTo :: Maybe JID
    , pId :: Maybe Text
             -- ^ Presence 'from', 'to', 'id' attributes
    , pType :: Maybe PresenceType
               -- ^ Presence type (2.2.1)
    , pShowType :: Maybe ShowType
                   -- ^ Show element (2.2.2.1)
    , pStatus :: Maybe Text
                 -- ^ Status element (2.2.2.2)
    , pPriority :: Maybe Int
                   -- ^ Presence priority (2.2.2.3)
    , pExt :: [Element]
              -- ^ Additional contents, used for extensions
    }

data IQ = IQ
    { iqFrom :: Maybe JID
    , iqTo :: Maybe JID
    , iqId :: Text
      -- ^ IQ id (Core-9.2.3)
    , iqType :: IQType
      -- ^ IQ type (Core-9.2.3)
    , iqBody :: Element
      -- ^ Child element (Core-9.2.3)
    }

data Stanza = SMessage Message | SPresence Presence | SIQ IQ --  deriving Show

data MessageType = Chat | GroupChat | Headline | Normal | MessageError deriving (Eq, Show)

data PresenceType = Default | Unavailable | Subscribe | Subscribed | Unsubscribe | Unsubscribed | Probe | PresenceError deriving Eq

data IQType = Get | Result | Set | IQError deriving Eq

data ShowType = Available | Away | FreeChat | DND | XAway deriving Eq

instance ToText MessageType where
  toText Chat = "chat"
  toText GroupChat = "groupchat"
  toText Headline = "headline"
  toText Normal = "normal"
  toText MessageError = "error"

instance ToText PresenceType where
  toText Default = ""
  toText Unavailable = "unavailable"
  toText Subscribe = "subscribe"
  toText Subscribed = "subscribed"
  toText Unsubscribe = "unsubscribe"
  toText Unsubscribed = "unsubscribed"
  toText Probe = "probe"
  toText PresenceError = "error"

instance ToText IQType where
  toText Get = "get"
  toText Result = "result"
  toText Set = "set"
  toText IQError = "error"

instance ToText ShowType where
  toText Available = ""
  toText Away = "away"
  toText FreeChat = "chat"
  toText DND = "dnd"
  toText XAway = "xa"


instance FromText MessageType where
  fromText "chat" = Chat
  fromText "groupchat" = GroupChat
  fromText "headline" = Headline
  fromText "normal" = Normal
  fromText "error" = MessageError
  fromText "" = Chat
  fromText _ = error "incorrect message type"

instance FromText PresenceType where
  fromText "" = Default
  fromText "available" = Default
  fromText "unavailable" = Unavailable
  fromText "subscribe" = Subscribe
  fromText "subscribed" = Subscribed
  fromText "unsubscribe" = Unsubscribe
  fromText "unsubscribed" = Unsubscribed
  fromText "probe" = Probe
  fromText "error" = PresenceError
  fromText _ = error "incorrect presence type"

instance FromText IQType where
  fromText "get" = Get
  fromText "result" = Result
  fromText "set" = Set
  fromText "error" = IQError
  fromText "" = Get
  fromText _ = error "incorrect iq type"

instance FromText ShowType where
  fromText "" = Available
  fromText "available" = Available
  fromText "away" = Away
  fromText "chat" = FreeChat
  fromText "dnd" = DND
  fromText "xa" = XAway
  fromText "invisible" = Available
  fromText _ = error "incorrect <show> value"

