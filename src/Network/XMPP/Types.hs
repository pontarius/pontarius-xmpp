module Network.XMPP.Types where
-- proudly "borrowed" from haskell-xmpp

import Control.Applicative((<$>))
import Control.Monad
import Control.Monad.Trans.State

import qualified Data.ByteString as BS
import Data.Conduit
import Data.List.Split as L
import Data.Maybe
import Data.Text as Text
import Data.String as Str

import System.IO

import Text.XML.Expat.SAX
import Text.XML.Expat.Tree

type Element = Node Text.Text Text.Text
type Event = SAXEvent Text.Text Text.Text

-- | Jabber ID (JID) datatype
data JID = JID { node :: Maybe Text
               -- ^ Account name
               , domain :: Text
               -- ^ Server adress
               , resource :: Maybe Text
               -- ^ Resource name
               }
instance Show JID where
  show (JID nd domain res) =
            maybe "" ((++ "@") . Text.unpack) nd ++
            (Text.unpack domain)               ++
            maybe "" (('/' :) . Text.unpack)   res

type XMPPMonad a = StateT XMPPState (ResourceT IO) a

data XMPPState = XMPPState
               { sConSrc    :: BufferedSource IO Event
               , sRawSrc    :: BufferedSource IO BS.ByteString
               , sConPush   :: BS.ByteString -> IO ()
               , sConHandle :: Maybe Handle
               , sFeatures  :: ServerFeatures
               , sHaveTLS   :: Bool
               , sHostname  :: Text.Text
               , sUsername  :: Text.Text
               , sResource  :: Maybe Text.Text
               }

data ServerFeatures = SF
  { stls  :: Maybe Bool
  , saslMechanisms :: [Text.Text]
  , other :: [Element]
  } deriving Show


def = SF
 { stls  = Nothing
 , saslMechanisms = []
 , other = []
 }


-- Ugh, that smells a bit.
parseJID jid =
  let (jid', rst) = case L.splitOn "@" jid of
                      [rest] -> (JID Nothing, rest)
                      [node,rest] -> (JID (Just (Text.pack node)), rest)
                      _ -> error $  "Couldn't parse JID: \"" ++ jid ++ "\""
  in case L.splitOn "/" rst of
      [domain] -> jid' (Text.pack domain) Nothing
      [domain, resource] -> jid' (Text.pack domain) (Just (Text.pack resource))
      _ -> error $ "Couldn't parse JID: \"" ++ jid ++ "\""

instance Read JID where
  readsPrec _ x = [(parseJID x,"")]


-- should we factor from, to and id out, even though they are
-- sometimes mandatory?
data Message = Message
    { mFrom :: Maybe JID
    , mTo :: JID
    , mId :: Maybe Text
             -- ^ Message 'from', 'to', 'id' attributes
    , mType :: Maybe MessageType
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
    } deriving Show

data IQ = IQ
    { iqFrom :: Maybe JID
    , iqTo :: Maybe JID
    , iqId :: Text
      -- ^ IQ id (Core-9.2.3)
    , iqType :: IQType
      -- ^ IQ type (Core-9.2.3)
    , iqBody :: Element
      -- ^ Child element (Core-9.2.3)
    } deriving Show

data Stanza = SMessage Message | SPresence Presence | SIQ IQ  deriving Show

data MessageType = Chat | GroupChat | Headline | Normal | MessageError deriving (Eq)

data PresenceType = Default | Unavailable | Subscribe | Subscribed | Unsubscribe | Unsubscribed | Probe | PresenceError deriving Eq

data IQType = Get | Result | Set | IQError deriving Eq

data ShowType = Available | Away | FreeChat | DND | XAway deriving Eq

instance Show MessageType where
  show Chat = "chat"
  show GroupChat = "groupchat"
  show Headline = "headline"
  show Normal = "normal"
  show MessageError = "error"

instance Show PresenceType where
  show Default = ""
  show Unavailable = "unavailable"
  show Subscribe = "subscribe"
  show Subscribed = "subscribed"
  show Unsubscribe = "unsubscribe"
  show Unsubscribed = "unsubscribed"
  show Probe = "probe"
  show PresenceError = "error"

instance Show IQType where
  show Get = "get"
  show Result = "result"
  show Set = "set"
  show IQError = "error"

instance Show ShowType where
  show Available = ""
  show Away = "away"
  show FreeChat = "chat"
  show DND = "dnd"
  show XAway = "xa"


instance Read MessageType where
  readsPrec _  "chat"         = [( Chat ,"")]
  readsPrec _  "groupchat"    = [( GroupChat ,"")]
  readsPrec _  "headline"     = [( Headline ,"")]
  readsPrec _  "normal"       = [( Normal ,"")]
  readsPrec _  "error"        = [( MessageError ,"")]
  readsPrec _  ""             = [( Chat ,"")]
  readsPrec _  _              = error "incorrect message type"

instance Read PresenceType where
  readsPrec _  ""             = [( Default ,"")]
  readsPrec _  "available"    = [( Default ,"")]
  readsPrec _  "unavailable"  = [( Unavailable ,"")]
  readsPrec _  "subscribe"    = [( Subscribe ,"")]
  readsPrec _  "subscribed"   = [( Subscribed ,"")]
  readsPrec _  "unsubscribe"  = [( Unsubscribe ,"")]
  readsPrec _  "unsubscribed" = [( Unsubscribed ,"")]
  readsPrec _  "probe"        = [( Probe ,"")]
  readsPrec _  "error"        = [( PresenceError ,"")]
  readsPrec _  _              =  error "incorrect presence type"

instance Read IQType where
  readsPrec _  "get"          = [( Get ,"")]
  readsPrec _  "result"       = [( Result ,"")]
  readsPrec _  "set"          = [( Set ,"")]
  readsPrec _  "error"        = [( IQError ,"")]
  readsPrec _  ""             = [( Get ,"")]
  readsPrec _  _              =  error "incorrect iq type"

instance Read ShowType where
  readsPrec _  ""             = [( Available ,"")]
  readsPrec _  "available"    = [( Available ,"")]
  readsPrec _  "away"         = [( Away ,"")]
  readsPrec _  "chat"         = [( FreeChat ,"")]
  readsPrec _  "dnd"          = [( DND ,"")]
  readsPrec _  "xa"           = [( XAway ,"")]
  readsPrec _  "invisible"    = [( Available ,"")]
  readsPrec _  _              =  error "incorrect <show> value"


toText :: Show a => a -> Text
toText   = Text.pack . show

fromText :: Read a => Text -> a
fromText = read . Text.unpack