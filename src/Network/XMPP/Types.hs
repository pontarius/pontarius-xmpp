{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_HADDOCK hide #-}

module Network.XMPP.Types
    ( IQError(..)
    , IQRequest(..)
    , IQRequestType(..)
    , IQResponse
    , IQResult(..)
    , IdGenerator(..)
    , LangTag (..)
    , Message(..)
    , MessageError(..)
    , MessageType(..)
    , Presence(..)
    , PresenceError(..)
    , PresenceType(..)
    , SaslError(..)
    , SaslFailure(..)
    , ServerFeatures(..)
    , Stanza(..)
    , StanzaError(..)
    , StanzaErrorCondition(..)
    , StanzaErrorType(..)
    , StanzaId(..)
    , StreamError(..)
    , Version(..)
    , XMPPConMonad
    , XmppConnection(..)
    , XmppConnectionState(..)
    , XMPPT(..)
    , XmppStreamError(..)
    , parseLangTag
    , module Network.XMPP.JID
    )
       where

import           Control.Applicative((<$>))
import           Control.Exception
import           Control.Monad.IO.Class
import           Control.Monad.State.Strict
import           Control.Monad.Error


import qualified Data.ByteString as BS
import           Data.Conduit
import           Data.String(IsString(..))
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Typeable(Typeable)
import           Data.XML.Types

import qualified Network as N

import           Network.XMPP.JID

import           System.IO

-- |
-- Wraps a string of random characters that, when using an appropriate
-- @IDGenerator@, is guaranteed to be unique for the XMPP session.

data StanzaId = SI !Text deriving (Eq, Ord)

instance Show StanzaId where
  show (SI s) = Text.unpack s

instance Read StanzaId where
  readsPrec _ x = [(SI $ Text.pack x, "")]

instance IsString StanzaId where
  fromString = SI . Text.pack

-- | The XMPP communication primities (Message, Presence and Info/Query) are
-- called stanzas.
data Stanza = IQRequestS     IQRequest
            | IQResultS      IQResult
            | IQErrorS       IQError
            | MessageS       Message
            | MessageErrorS  MessageError
            | PresenceS      Presence
            | PresenceErrorS PresenceError
              deriving Show

-- | A "request" Info/Query (IQ) stanza is one with either "get" or "set" as
-- type. They are guaranteed to always contain a payload.
data IQRequest = IQRequest { iqRequestID      :: StanzaId
                           , iqRequestFrom    :: Maybe JID
                           , iqRequestTo      :: Maybe JID
                           , iqRequestLangTag :: Maybe LangTag
                           , iqRequestType    :: IQRequestType
                           , iqRequestPayload :: Element
                           } deriving Show

-- | The type of IQ request that is made.
data IQRequestType = Get | Set deriving (Eq, Ord)

instance Show IQRequestType where
  show Get = "get"
  show Set = "set"

instance Read IQRequestType where
  readsPrec _ "get" = [(Get, "")]
  readsPrec _ "set" = [(Set, "")]
  readsPrec _ _ = []

-- | A "response" Info/Query (IQ) stanza is either an 'IQError' or an IQ stanza
-- with the type "result" ('IQResult').
type IQResponse = Either IQError IQResult

-- | The (non-error) answer to an IQ request.
data IQResult = IQResult { iqResultID :: StanzaId
                         , iqResultFrom :: Maybe JID
                         , iqResultTo :: Maybe JID
                         , iqResultLangTag :: Maybe LangTag
                         , iqResultPayload :: Maybe Element
                         } deriving Show

-- | The answer to an IQ request that generated an error.
data IQError = IQError { iqErrorID :: StanzaId
                       , iqErrorFrom :: Maybe JID
                       , iqErrorTo :: Maybe JID
                       , iqErrorLangTag :: Maybe LangTag
                       , iqErrorStanzaError :: StanzaError
                       , iqErrorPayload :: Maybe Element -- should this be []?
                       } deriving Show

-- | The message stanza. Used for /push/ type communication.
data Message = Message { messageID      :: Maybe StanzaId
                       , messageFrom    :: Maybe JID
                       , messageTo      :: Maybe JID
                       , messageLangTag :: Maybe LangTag
                       , messageType    :: MessageType
                       , messagePayload :: [Element]
                       } deriving Show

-- | An error stanza generated in response to a 'Message'.
data MessageError = MessageError { messageErrorID :: Maybe StanzaId
                                 , messageErrorFrom :: Maybe JID
                                 , messageErrorTo :: Maybe JID
                                 , messageErrorLangTag  :: Maybe LangTag
                                 , messageErrorStanzaError :: StanzaError
                                 , messageErrorPayload :: [Element]
                                 } deriving (Show)


-- | The type of a Message being sent
-- (<http://xmpp.org/rfcs/rfc6121.html#message-syntax-type>)
data MessageType = -- | The message is sent in the context of a one-to-one chat
                   -- session. Typically an interactive client will present a
                   -- message of type /chat/ in an interface that enables
                   -- one-to-one chat between the two parties, including an
                   -- appropriate conversation history.
                   Chat
                   -- | The message is sent in the context of a multi-user chat
                   -- environment (similar to that of @IRC@). Typically a
                   -- receiving client will present a message of type
                   -- /groupchat/ in an interface that enables many-to-many
                   -- chat between the parties, including a roster of parties
                   -- in the chatroom and an appropriate conversation history.
                 | GroupChat
                   -- | The message provides an alert, a notification, or other
                   -- transient information to which no reply is expected
                   -- (e.g., news headlines, sports updates, near-real-time
                   -- market data, or syndicated content). Because no reply to
                   -- the message is expected, typically a receiving client
                   -- will present a message of type /headline/ in an interface
                   -- that appropriately differentiates the message from
                   -- standalone messages, chat messages, and groupchat
                   -- messages (e.g., by not providing the recipient with the
                   -- ability to reply).
                 | Headline
                   -- | The message is a standalone message that is sent outside
                   -- the context of a one-to-one conversation or groupchat, and
                   -- to which it is expected that the recipient will reply.
                   -- Typically a receiving client will present a message of
                   -- type /normal/ in an interface that enables the recipient
                   -- to reply, but without a conversation history.
                   --
                   -- This is the /default/ value.
                 | Normal
                 deriving (Eq)

instance Show MessageType where
    show Chat = "chat"
    show GroupChat = "groupchat"
    show Headline = "headline"
    show Normal = "normal"

instance Read MessageType where
    readsPrec _  "chat"      = [(Chat, "")]
    readsPrec _  "groupchat" = [(GroupChat, "")]
    readsPrec _  "headline"  = [(Headline, "")]
    readsPrec _  "normal"    = [(Normal, "")]
    readsPrec _  _           = [(Normal, "")]

-- | The presence stanza. Used for communicating status updates.
data Presence = Presence { presenceID      :: Maybe StanzaId
                         , presenceFrom    :: Maybe JID
                         , presenceTo      :: Maybe JID
                         , presenceLangTag :: Maybe LangTag
                         , presenceType    :: Maybe PresenceType
                         , presencePayload :: [Element]
                         } deriving Show


-- | An error stanza generated in response to a 'Presence'.
data PresenceError = PresenceError { presenceErrorID          :: Maybe StanzaId
                                   , presenceErrorFrom        :: Maybe JID
                                   , presenceErrorTo          :: Maybe JID
                                   , presenceErrorLangTag     :: Maybe LangTag
                                   , presenceErrorStanzaError :: StanzaError
                                   , presenceErrorPayload     :: [Element]
                                   } deriving Show

-- | @PresenceType@ holds XMPP presence types. The "error" message type is left
-- out as errors are using @PresenceError@.
data PresenceType = Subscribe    | -- ^ Sender wants to subscribe to presence
                    Subscribed   | -- ^ Sender has approved the subscription
                    Unsubscribe  | -- ^ Sender is unsubscribing from presence
                    Unsubscribed | -- ^ Sender has denied or cancelled a
                                   --   subscription
                    Probe        | -- ^ Sender requests current presence;
                                   --   should only be used by servers
                    Default      |
                    Unavailable deriving (Eq)

instance Show PresenceType where
    show Subscribe    = "subscribe"
    show Subscribed   = "subscribed"
    show Unsubscribe  = "unsubscribe"
    show Unsubscribed = "unsubscribed"
    show Probe        = "probe"
    show Default      = ""
    show Unavailable  = "unavailable"

instance Read PresenceType where
    readsPrec _  ""             = [(Default, "")]
    readsPrec _  "available"    = [(Default, "")]
    readsPrec _  "unavailable"  = [(Unavailable, "")]
    readsPrec _  "subscribe"    = [(Subscribe, "")]
    readsPrec _  "subscribed"   = [(Subscribed, "")]
    readsPrec _  "unsubscribe"  = [(Unsubscribe, "")]
    readsPrec _  "unsubscribed" = [(Unsubscribed, "")]
    readsPrec _  "probe"        = [(Probe, "")]
    readsPrec _  _              = []

--data ShowType = Available
--                | Away
--                | FreeChat
--                | DND
--                | XAway
--                deriving Eq
--
--instance Show ShowType where
--  show Available = ""
--  show Away = "away"
--  show FreeChat = "chat"
--  show DND = "dnd"
--  show XAway = "xa"
--
--instance Read ShowType where
--  readsPrec _  ""             = [( Available ,"")]
--  readsPrec _  "available"    = [( Available ,"")]
--  readsPrec _  "away"         = [( Away ,"")]
--  readsPrec _  "chat"         = [( FreeChat ,"")]
--  readsPrec _  "dnd"          = [( DND ,"")]
--  readsPrec _  "xa"           = [( XAway ,"")]
--  readsPrec _  "invisible"    = [( Available ,"")]
--  readsPrec _  _              = []


-- | All stanzas (IQ, message, presence) can cause errors, which in the XMPP
-- stream looks like <stanza-kind to='sender' type='error'>. These errors are
-- wrapped in the @StanzaError@ type.
-- TODO: Sender XML is (optional and is) not yet included.
data StanzaError = StanzaError
    { stanzaErrorType :: StanzaErrorType
    , stanzaErrorCondition :: StanzaErrorCondition
    , stanzaErrorText :: Maybe (Maybe LangTag, Text)
    , stanzaErrorApplicationSpecificCondition :: Maybe Element
    } deriving (Eq, Show)

-- | @StanzaError@s always have one of these types.
data StanzaErrorType = Cancel   | -- ^ Error is unrecoverable - do not retry
                       Continue | -- ^ Conditition was a warning - proceed
                       Modify   | -- ^ Change the data and retry
                       Auth     | -- ^ Provide credentials and retry
                       Wait       -- ^ Error is temporary - wait and retry
                       deriving (Eq)

instance Show StanzaErrorType where
    show Cancel = "cancel"
    show Continue = "continue"
    show Modify = "modify"
    show Auth = "auth"
    show Wait = "wait"

instance Read StanzaErrorType where
  readsPrec _ "auth"     = [( Auth    , "")]
  readsPrec _ "cancel"   = [( Cancel  , "")]
  readsPrec _ "continue" = [( Continue, "")]
  readsPrec _ "modify"   = [( Modify  , "")]
  readsPrec _ "wait"     = [( Wait    , "")]
  readsPrec _ _          = []

-- | Stanza errors are accommodated with one of the error conditions listed
-- below.
data StanzaErrorCondition = BadRequest            -- ^ Malformed XML.
                          | Conflict              -- ^ Resource or session with
                                                  --   name already exists.
                          | FeatureNotImplemented 
                          | Forbidden             -- ^ Insufficient permissions.
                          | Gone                  -- ^ Entity can no longer be
                                                  --   contacted at this
                                                  --   address.
                          | InternalServerError
                          | ItemNotFound         
                          | JIDMalformed         
                          | NotAcceptable         -- ^ Does not meet policy
                                                  --   criteria.
                          | NotAllowed            -- ^ No entity may perform
                                                  --   this action.
                          | NotAuthorized         -- ^ Must provide proper
                                                  --   credentials.
                          | PaymentRequired      
                          | RecipientUnavailable  -- ^ Temporarily unavailable.
                          | Redirect              -- ^ Redirecting to other
                                                  --   entity, usually
                                                  --   temporarily.
                          | RegistrationRequired 
                          | RemoteServerNotFound 
                          | RemoteServerTimeout  
                          | ResourceConstraint    -- ^ Entity lacks the
                                                  --   necessary system
                                                  --   resources.
                          | ServiceUnavailable   
                          | SubscriptionRequired 
                          | UndefinedCondition    -- ^ Application-specific
                                                  --   condition.
                          | UnexpectedRequest     -- ^ Badly timed request.
                            deriving Eq

instance Show StanzaErrorCondition where
    show BadRequest = "bad-request"
    show Conflict = "conflict"
    show FeatureNotImplemented = "feature-not-implemented"
    show Forbidden = "forbidden"
    show Gone = "gone"
    show InternalServerError = "internal-server-error"
    show ItemNotFound = "item-not-found"
    show JIDMalformed = "jid-malformed"
    show NotAcceptable = "not-acceptable"
    show NotAllowed = "not-allowed"
    show NotAuthorized = "not-authorized"
    show PaymentRequired = "payment-required"
    show RecipientUnavailable = "recipient-unavailable"
    show Redirect = "redirect"
    show RegistrationRequired = "registration-required"
    show RemoteServerNotFound = "remote-server-not-found"
    show RemoteServerTimeout = "remote-server-timeout"
    show ResourceConstraint = "resource-constraint"
    show ServiceUnavailable = "service-unavailable"
    show SubscriptionRequired = "subscription-required"
    show UndefinedCondition = "undefined-condition"
    show UnexpectedRequest = "unexpected-request"

instance Read StanzaErrorCondition where
    readsPrec _  "bad-request"             = [(BadRequest           , "")]
    readsPrec _  "conflict"                = [(Conflict             , "")]
    readsPrec _  "feature-not-implemented" = [(FeatureNotImplemented, "")]
    readsPrec _  "forbidden"               = [(Forbidden            , "")]
    readsPrec _  "gone"                    = [(Gone                 , "")]
    readsPrec _  "internal-server-error"   = [(InternalServerError  , "")]
    readsPrec _  "item-not-found"          = [(ItemNotFound         , "")]
    readsPrec _  "jid-malformed"           = [(JIDMalformed         , "")]
    readsPrec _  "not-acceptable"          = [(NotAcceptable        , "")]
    readsPrec _  "not-allowed"             = [(NotAllowed           , "")]
    readsPrec _  "not-authorized"          = [(NotAuthorized        , "")]
    readsPrec _  "payment-required"        = [(PaymentRequired      , "")]
    readsPrec _  "recipient-unavailable"   = [(RecipientUnavailable , "")]
    readsPrec _  "redirect"                = [(Redirect             , "")]
    readsPrec _  "registration-required"   = [(RegistrationRequired , "")]
    readsPrec _  "remote-server-not-found" = [(RemoteServerNotFound , "")]
    readsPrec _  "remote-server-timeout"   = [(RemoteServerTimeout  , "")]
    readsPrec _  "resource-constraint"     = [(ResourceConstraint   , "")]
    readsPrec _  "service-unavailable"     = [(ServiceUnavailable   , "")]
    readsPrec _  "subscription-required"   = [(SubscriptionRequired , "")]
    readsPrec _  "unexpected-request"      = [(UnexpectedRequest    , "")]
    readsPrec _  "undefined-condition"     = [(UndefinedCondition   , "")]
    readsPrec _  _                         = [(UndefinedCondition   , "")]

-- =============================================================================
--  OTHER STUFF
-- =============================================================================

data SaslFailure = SaslFailure { saslFailureCondition :: SaslError
                               , saslFailureText :: Maybe ( Maybe LangTag
                                                          , Text
                                                          )
                               } deriving Show

data SaslError = SaslAborted              -- ^ Client aborted.
               | SaslAccountDisabled      -- ^ The account has been temporarily
                                          --   disabled.
               | SaslCredentialsExpired   -- ^ The authentication failed because
                                          --   the credentials have expired.
               | SaslEncryptionRequired   -- ^ The mechanism requested cannot be
                                          --   used the confidentiality and
                                          --   integrity of the underlying
                                          --   stream is protected (typically
                                          --   with TLS).
               | SaslIncorrectEncoding    -- ^ The base64 encoding is incorrect.
               | SaslInvalidAuthzid       -- ^ The authzid has an incorrect
                                          --   format or the initiating entity
                                          --   does not have the appropriate
                                          --   permissions to authorize that ID.
               | SaslInvalidMechanism     -- ^ The mechanism is not supported by
                                          --   the receiving entity.
               | SaslMalformedRequest     -- ^ Invalid syntax.
               | SaslMechanismTooWeak     -- ^ The receiving entity policy
                                          --   requires a stronger mechanism.
               | SaslNotAuthorized        -- ^ Invalid credentials provided, or
                                          --   some generic authentication
                                          --   failure has occurred.
               | SaslTemporaryAuthFailure -- ^ There receiving entity reported a
                                          --   temporary error condition; the
                                          --   initiating entity is recommended
                                          --   to try again later.

instance Show SaslError where
    show SaslAborted               = "aborted"
    show SaslAccountDisabled       = "account-disabled"
    show SaslCredentialsExpired    = "credentials-expired"
    show SaslEncryptionRequired    = "encryption-required"
    show SaslIncorrectEncoding     = "incorrect-encoding"
    show SaslInvalidAuthzid        = "invalid-authzid"
    show SaslInvalidMechanism      = "invalid-mechanism"
    show SaslMalformedRequest      = "malformed-request"
    show SaslMechanismTooWeak      = "mechanism-too-weak"
    show SaslNotAuthorized         = "not-authorized"
    show SaslTemporaryAuthFailure  = "temporary-auth-failure"

instance Read SaslError where
    readsPrec _ "aborted"                = [(SaslAborted              , "")]
    readsPrec _ "account-disabled"       = [(SaslAccountDisabled      , "")]
    readsPrec _ "credentials-expired"    = [(SaslCredentialsExpired   , "")]
    readsPrec _ "encryption-required"    = [(SaslEncryptionRequired   , "")]
    readsPrec _ "incorrect-encoding"     = [(SaslIncorrectEncoding    , "")]
    readsPrec _ "invalid-authzid"        = [(SaslInvalidAuthzid       , "")]
    readsPrec _ "invalid-mechanism"      = [(SaslInvalidMechanism     , "")]
    readsPrec _ "malformed-request"      = [(SaslMalformedRequest     , "")]
    readsPrec _ "mechanism-too-weak"     = [(SaslMechanismTooWeak     , "")]
    readsPrec _ "not-authorized"         = [(SaslNotAuthorized        , "")]
    readsPrec _ "temporary-auth-failure" = [(SaslTemporaryAuthFailure , "")]
    readsPrec _ _                        = []

-- data ServerAddress = ServerAddress N.HostName N.PortNumber deriving (Eq)

-- TODO: document the error cases
data StreamErrorCondition
    = StreamBadFormat
    | StreamBadNamespacePrefix
    | StreamConflict
    | StreamConnectionTimeout
    | StreamHostGone
    | StreamHostUnknown
    | StreamImproperAddressing
    | StreamInternalServerError
    | StreamInvalidFrom
    | StreamInvalidNamespace
    | StreamInvalidXml
    | StreamNotAuthorized
    | StreamNotWellFormed
    | StreamPolicyViolation
    | StreamRemoteConnectionFailed
    | StreamReset
    | StreamResourceConstraint
    | StreamRestrictedXml
    | StreamSeeOtherHost
    | StreamSystemShutdown
    | StreamUndefinedCondition
    | StreamUnsupportedEncoding
    | StreamUnsupportedFeature
    | StreamUnsupportedStanzaType
    | StreamUnsupportedVersion
      deriving Eq

instance Show StreamErrorCondition where
    show StreamBadFormat              = "bad-format"
    show StreamBadNamespacePrefix     = "bad-namespace-prefix"
    show StreamConflict               = "conflict"
    show StreamConnectionTimeout      = "connection-timeout"
    show StreamHostGone               = "host-gone"
    show StreamHostUnknown            = "host-unknown"
    show StreamImproperAddressing     = "improper-addressing"
    show StreamInternalServerError    = "internal-server-error"
    show StreamInvalidFrom            = "invalid-from"
    show StreamInvalidNamespace       = "invalid-namespace"
    show StreamInvalidXml             = "invalid-xml"
    show StreamNotAuthorized          = "not-authorized"
    show StreamNotWellFormed          = "not-well-formed"
    show StreamPolicyViolation        = "policy-violation"
    show StreamRemoteConnectionFailed = "remote-connection-failed"
    show StreamReset                  = "reset"
    show StreamResourceConstraint     = "resource-constraint"
    show StreamRestrictedXml          = "restricted-xml"
    show StreamSeeOtherHost           = "see-other-host"
    show StreamSystemShutdown         = "system-shutdown"
    show StreamUndefinedCondition     = "undefined-condition"
    show StreamUnsupportedEncoding    = "unsupported-encoding"
    show StreamUnsupportedFeature     = "unsupported-feature"
    show StreamUnsupportedStanzaType  = "unsupported-stanza-type"
    show StreamUnsupportedVersion     = "unsupported-version"

instance Read StreamErrorCondition where
    readsPrec _ "bad-format"               = [(StreamBadFormat            , "")]
    readsPrec _ "bad-namespace-prefix"     = [(StreamBadNamespacePrefix   , "")]
    readsPrec _ "conflict"                 = [(StreamConflict             , "")]
    readsPrec _ "connection-timeout"       = [(StreamConnectionTimeout    , "")]
    readsPrec _ "host-gone"                = [(StreamHostGone             , "")]
    readsPrec _ "host-unknown"             = [(StreamHostUnknown          , "")]
    readsPrec _ "improper-addressing"      = [(StreamImproperAddressing   , "")]
    readsPrec _ "internal-server-error"    = [(StreamInternalServerError  , "")]
    readsPrec _ "invalid-from"             = [(StreamInvalidFrom          , "")]
    readsPrec _ "invalid-namespace"        = [(StreamInvalidNamespace     , "")]
    readsPrec _ "invalid-xml"              = [(StreamInvalidXml           , "")]
    readsPrec _ "not-authorized"           = [(StreamNotAuthorized        , "")]
    readsPrec _ "not-well-formed"          = [(StreamNotWellFormed        , "")]
    readsPrec _ "policy-violation"         = [(StreamPolicyViolation      , "")]
    readsPrec _ "remote-connection-failed" =
        [(StreamRemoteConnectionFailed, "")]
    readsPrec _ "reset"                    = [(StreamReset                , "")]
    readsPrec _ "resource-constraint"      = [(StreamResourceConstraint   , "")]
    readsPrec _ "restricted-xml"           = [(StreamRestrictedXml        , "")]
    readsPrec _ "see-other-host"           = [(StreamSeeOtherHost         , "")]
    readsPrec _ "system-shutdown"          = [(StreamSystemShutdown       , "")]
    readsPrec _ "undefined-condition"      = [(StreamUndefinedCondition   , "")]
    readsPrec _ "unsupported-encoding"     = [(StreamUnsupportedEncoding  , "")]
    readsPrec _ "unsupported-feature"      = [(StreamUnsupportedFeature   , "")]
    readsPrec _ "unsupported-stanza-type"  = [(StreamUnsupportedStanzaType, "")]
    readsPrec _ "unsupported-version"      = [(StreamUnsupportedVersion   , "")]
    readsPrec _ _                          = [(StreamUndefinedCondition   , "")]

data XmppStreamError = XmppStreamError
    { errorCondition :: StreamErrorCondition
    , errorText :: Maybe (Maybe LangTag, Text)
    , errorXML :: Maybe Element
    } deriving (Show, Eq)

data StreamError = StreamError XmppStreamError
                 | StreamWrongVersion Text
                 | StreamXMLError String -- If stream pickling goes wrong.
                 | StreamConnectionError
                 deriving (Show, Eq, Typeable)

instance Exception StreamError
instance Error StreamError where noMsg = StreamConnectionError

-- =============================================================================
--  XML TYPES
-- =============================================================================


-- | Wraps a function that MUST generate a stream of unique Ids. The
--   strings MUST be appropriate for use in the stanza id attirubte.
--   For a default implementation, see @idGenerator@.

newtype IdGenerator = IdGenerator (IO Text)


-- Version numbers are displayed as "<major>.<minor>".

data Version = Version { majorVersion :: Integer
                       , minorVersion :: Integer } deriving (Eq)

instance Show Version where
    show (Version major minor) = (show major) ++ "." ++ (show minor)

-- If the major version numbers are not equal, compare them. Otherwise, compare
-- the minor version numbers.
instance Ord Version where
    compare (Version amajor aminor) (Version bmajor bminor)
        | amajor /= bmajor = compare amajor bmajor
        | otherwise = compare aminor bminor

-- The language tag in the form of "en-US". It has a primary tag, followed by a
-- number of subtags.
data LangTag = LangTag { primaryTag :: Text
                       , subtags :: [Text] }
                        deriving (Eq) -- TODO: remove

instance Show LangTag where
    show (LangTag p []) = Text.unpack p
    show (LangTag p s) = Text.unpack . Text.concat
                           $ [p, "-", Text.intercalate "-" s] -- TODO: clean up

-- Parses a Text string to a list of LangTag objects. TODO: Why?
parseLangTag :: Text -> [LangTag]
parseLangTag txt = case Text.splitOn "-" txt of
  [] -> []
  prim: subs -> [LangTag prim subs]

instance Read LangTag where
  readsPrec _ txt = (,"") <$> (parseLangTag $ Text.pack txt)

-- Two language tags are considered equal of they contain the same tags
-- (case-insensitive).

-- instance Eq LangTag where
--   (LangTag ap as) == (LangTag bp bs)
--         | length as == length bs && map toLower ap == map toLower bp =
--           all (\ (a, b) -> map toLower a == map toLower b) $ zip as bs
--         | otherwise = False

data ServerFeatures = SF
    { stls  :: Maybe Bool
    , saslMechanisms :: [Text.Text]
    , other :: [Element]
    } deriving Show

data XmppConnectionState
    = XmppConnectionClosed  -- ^ No connection at this point.
    | XmppConnectionPlain   -- ^ Connection established, but not secured.
    | XmppConnectionSecured -- ^ Connection established and secured via TLS.
      deriving (Show, Eq, Typeable)

data XmppConnection = XmppConnection
               { sConSrc          :: Source IO Event
               , sRawSrc          :: Source IO BS.ByteString
               , sConPushBS       :: BS.ByteString -> IO Bool
               , sConHandle       :: Maybe Handle
               , sFeatures        :: ServerFeatures
               , sConnectionState :: XmppConnectionState
               , sHostname        :: Maybe Text
               , sUsername        :: Maybe Text
               , sResource        :: Maybe Text
               , sCloseConnection :: IO ()
                 -- TODO: add default Language
               }

-- |
-- The XMPP monad transformer. Contains internal state in order to
-- work with Pontarius. Pontarius clients needs to operate in this
-- context.
newtype XMPPT m a = XMPPT { runXMPPT :: StateT XmppConnection m a } deriving (Monad, MonadIO)

-- | Low-level and single-threaded XMPP monad. See @XMPP@ for a concurrent
-- implementation.
type XMPPConMonad a = StateT XmppConnection IO a

-- Make XMPPT derive the Monad and MonadIO instances.
deriving instance (Monad m, MonadIO m) => MonadState (XmppConnection) (XMPPT m)