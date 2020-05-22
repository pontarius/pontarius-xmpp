{-# LANGUAGE CPP #-}

#if WITH_TEMPLATE_HASKELL
{-# LANGUAGE TemplateHaskell #-}
#endif

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_HADDOCK hide #-}

module Network.Xmpp.Types
    ( NonemptyText(..)
    , nonEmpty
    , text
    , IQError(..)
    , IQRequest(..)
    , IQRequestType(..)
    , IQResponse(..)
    , IQResult(..)
    , LangTag (..)
#if WITH_TEMPLATE_HASKELL
    , langTagQ
#endif
    , langTagFromText
    , langTagToText
    , parseLangTag
    , ExtendedAttribute
    , Message(..)
    , message
    , MessageError(..)
    , messageError
    , MessageType(..)
    , Presence(..)
    , presence
    , PresenceError(..)
    , PresenceType(..)
    , SaslError(..)
    , SaslFailure(..)
    , StreamFeatures(..)
    , Stanza(..)
    , XmppElement(..)
    , messageS
    , messageErrorS
    , presenceS
    , StanzaError(..)
    , StanzaErrorCondition(..)
    , StanzaErrorType(..)
    , XmppFailure(..)
    , XmppTlsError(..)
    , StreamErrorCondition(..)
    , Version(..)
    , versionFromText
    , StreamHandle(..)
    , Stream(..)
    , StreamState(..)
    , ConnectionState(..)
    , StreamErrorInfo(..)
    , ConnectionDetails(..)
    , StreamConfiguration(..)
    , xmppDefaultParams
    , xmppDefaultParamsStrong
    , Jid(..)
#if WITH_TEMPLATE_HASKELL
    , jidQ
    , jid
#endif
    , isBare
    , isFull
    , jidFromText
    , jidFromTexts
    , (<~)
    , nodeprepProfile
    , resourceprepProfile
    , jidToText
    , jidToTexts
    , toBare
    , localpart
    , domainpart
    , resourcepart
    , parseJid
    , TlsBehaviour(..)
    , AuthFailure(..)
    ) where

import           Control.Applicative ((<$>), (<|>), many)
import           Control.Concurrent.STM
import           Control.Exception
import           Control.Monad.Except
import qualified Data.Attoparsec.Text as AP
import qualified Data.ByteString as BS
import           Data.Char (isSpace)
import           Data.Conduit
import           Data.Default
import           Data.Semigroup as Sem
import qualified Data.Set as Set
import           Data.String (IsString, fromString)
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Typeable(Typeable)
import           Data.XML.Types as XML
import qualified Data.Text.Encoding as Text
#if WITH_TEMPLATE_HASKELL
import           Language.Haskell.TH
import           Language.Haskell.TH.Quote
import qualified Language.Haskell.TH.Syntax as TH
#endif
import           Network.Socket
import           Network.DNS
import           Network.TLS hiding (Version, HostName)
import           Network.TLS.Extra
import qualified Text.StringPrep as SP
import qualified Text.StringPrep.Profiles as SP


-- $setup
-- :set -itests
-- >>> :add tests/Tests/Arbitrary.hs
-- >>> import Network.Xmpp.Types
-- >>> import Control.Applicative((<$>))

-- | Type of Texts that contain at least on non-space character
newtype NonemptyText = Nonempty {fromNonempty :: Text}
                       deriving (Show, Read, Eq, Ord)

instance IsString NonemptyText where
    fromString str = case nonEmpty (Text.pack str) of
        Nothing -> error $ "NonemptyText fromString called on empty or " ++
                            "all-whitespace string"
        Just r -> r

-- | Check that Text contains at least one non-space character and wrap it
nonEmpty :: Text -> Maybe NonemptyText
nonEmpty txt = if Text.all isSpace txt then Nothing else Just (Nonempty txt)

-- | Same as 'fromNonempty'
text :: NonemptyText -> Text
text (Nonempty txt) = txt

data XmppElement = XmppStanza !Stanza
                 | XmppNonza  !Element
                   deriving (Eq, Show)

-- | The Xmpp communication primitives (Message, Presence and Info/Query) are
-- called stanzas.
data Stanza = IQRequestS     !IQRequest
            | IQResultS      !IQResult
            | IQErrorS       !IQError
            | MessageS       !Message
            | MessageErrorS  !MessageError
            | PresenceS      !Presence
            | PresenceErrorS !PresenceError
              deriving (Eq, Show)

type ExtendedAttribute = (XML.Name, Text)

-- | A "request" Info/Query (IQ) stanza is one with either "get" or "set" as
-- type. It always contains an xml payload.
data IQRequest = IQRequest { iqRequestID      :: !Text
                           , iqRequestFrom    :: !(Maybe Jid)
                           , iqRequestTo      :: !(Maybe Jid)
                           , iqRequestLangTag :: !(Maybe LangTag)
                           , iqRequestType    :: !IQRequestType
                           , iqRequestPayload :: !Element
                           , iqRequestAttributes :: ![ExtendedAttribute]
                           } deriving (Eq, Show)

-- | The type of IQ request that is made.
data IQRequestType = Get | Set deriving (Eq, Ord, Read, Show)

-- | A "response" Info/Query (IQ) stanza is either an 'IQError', an IQ stanza
-- of  type "result" ('IQResult')
data IQResponse = IQResponseError IQError
                | IQResponseResult IQResult
                deriving (Eq, Show)

-- | The (non-error) answer to an IQ request.
data IQResult = IQResult { iqResultID      :: !Text
                         , iqResultFrom    :: !(Maybe Jid)
                         , iqResultTo      :: !(Maybe Jid)
                         , iqResultLangTag :: !(Maybe LangTag)
                         , iqResultPayload :: !(Maybe Element)
                         , iqResultAttributes :: ![ExtendedAttribute]
                         } deriving (Eq, Show)

-- | The answer to an IQ request that generated an error.
data IQError = IQError { iqErrorID          :: !Text
                       , iqErrorFrom        :: !(Maybe Jid)
                       , iqErrorTo          :: !(Maybe Jid)
                       , iqErrorLangTag     :: !(Maybe LangTag)
                       , iqErrorStanzaError :: !StanzaError
                       , iqErrorPayload     :: !(Maybe Element) -- should this be []?
                       , iqErrorAttributes  :: ![ExtendedAttribute]
                       } deriving (Eq, Show)

-- | The message stanza. Used for /push/ type communication.
data Message = Message { messageID      :: !(Maybe Text)
                       , messageFrom    :: !(Maybe Jid)
                       , messageTo      :: !(Maybe Jid)
                       , messageLangTag :: !(Maybe LangTag)
                       , messageType    :: !MessageType
                       , messagePayload :: ![Element]
                       , messageAttributes :: ![ExtendedAttribute]
                       } deriving (Eq, Show)

-- | An empty message
--
-- @
-- message = Message { messageID      = Nothing
--                   , messageFrom    = Nothing
--                   , messageTo      = Nothing
--                   , messageLangTag = Nothing
--                   , messageType    = Normal
--                   , messagePayload = []
--                   }
-- @
message :: Message
message = Message { messageID      = Nothing
                  , messageFrom    = Nothing
                  , messageTo      = Nothing
                  , messageLangTag = Nothing
                  , messageType    = Normal
                  , messagePayload = []
                  , messageAttributes = []
                  }

-- | Empty message stanza
--
-- @messageS = 'MessageS' 'message'@
messageS :: Stanza
messageS = MessageS message

instance Default Message where
    def = message

-- | An error stanza generated in response to a 'Message'.
data MessageError = MessageError { messageErrorID          :: !(Maybe Text)
                                 , messageErrorFrom        :: !(Maybe Jid)
                                 , messageErrorTo          :: !(Maybe Jid)
                                 , messageErrorLangTag     :: !(Maybe LangTag)
                                 , messageErrorStanzaError :: !StanzaError
                                 , messageErrorPayload     :: ![Element]
                                 , messageErrorAttributes  :: ![ExtendedAttribute]
                                 } deriving (Eq, Show)

messageError :: MessageError
messageError = MessageError { messageErrorID          = Nothing
                            , messageErrorFrom        = Nothing
                            , messageErrorTo          = Nothing
                            , messageErrorLangTag     = Nothing
                            , messageErrorStanzaError =
                                StanzaError { stanzaErrorType = Cancel
                                            , stanzaErrorCondition =
                                                  ServiceUnavailable
                                            , stanzaErrorText = Nothing
                                            , stanzaErrorApplicationSpecificCondition = Nothing
                                            }
                            , messageErrorPayload     = []
                            , messageErrorAttributes  = []
                            }

instance Default MessageError where
    def = messageError

messageErrorS :: Stanza
messageErrorS = MessageErrorS def

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
                 deriving (Eq, Read, Show)

-- | The presence stanza. Used for communicating status updates.
data Presence = Presence { presenceID      :: !(Maybe Text)
                         , presenceFrom    :: !(Maybe Jid)
                         , presenceTo      :: !(Maybe Jid)
                         , presenceLangTag :: !(Maybe LangTag)
                         , presenceType    :: !PresenceType
                         , presencePayload :: ![Element]
                         , presenceAttributes :: ![ExtendedAttribute]
                         } deriving (Eq, Show)

-- | An empty presence.
presence :: Presence
presence = Presence { presenceID       = Nothing
                    , presenceFrom     = Nothing
                    , presenceTo       = Nothing
                    , presenceLangTag  = Nothing
                    , presenceType     = Available
                    , presencePayload  = []
                    , presenceAttributes = []
                    }

-- | Empty presence stanza
presenceS :: Stanza
presenceS = PresenceS presence

instance Default Presence where
    def = presence

-- | An error stanza generated in response to a 'Presence'.
data PresenceError = PresenceError { presenceErrorID          :: !(Maybe Text)
                                   , presenceErrorFrom        :: !(Maybe Jid)
                                   , presenceErrorTo          :: !(Maybe Jid)
                                   , presenceErrorLangTag     :: !(Maybe LangTag)
                                   , presenceErrorStanzaError :: !StanzaError
                                   , presenceErrorPayload     :: ![Element]
                                   , presenceErrorAttributes  :: ![ExtendedAttribute]
                                   } deriving (Eq, Show)

-- | @PresenceType@ holds Xmpp presence types. The "error" message type is left
-- out as errors are using @PresenceError@.
data PresenceType = Subscribe    | -- ^ Sender wants to subscribe to presence
                    Subscribed   | -- ^ Sender has approved the subscription
                    Unsubscribe  | -- ^ Sender is unsubscribing from presence
                    Unsubscribed | -- ^ Sender has denied or cancelled a
                                   --   subscription
                    Probe        | -- ^ Sender requests current presence;
                                   --   should only be used by servers
                    Available    | -- ^ Sender wants to express availability
                                   --   (no type attribute is defined)
                    Unavailable deriving (Eq, Read, Show)

-- | All stanzas (IQ, message, presence) can cause errors, which in the Xmpp
-- stream looks like @\<stanza-kind to=\'sender\' type=\'error\'\>@ . These
-- errors are wrapped in the @StanzaError@ type.  TODO: Sender XML is (optional
-- and is) not yet included.
data StanzaError = StanzaError
    { stanzaErrorType                         :: StanzaErrorType
    , stanzaErrorCondition                    :: StanzaErrorCondition
    , stanzaErrorText                         :: Maybe (Maybe LangTag, NonemptyText)
    , stanzaErrorApplicationSpecificCondition :: Maybe Element
    } deriving (Eq, Show)

-- | @StanzaError@s always have one of these types.
data StanzaErrorType = Cancel   | -- ^ Error is unrecoverable - do not retry
                       Continue | -- ^ Conditition was a warning - proceed
                       Modify   | -- ^ Change the data and retry
                       Auth     | -- ^ Provide credentials and retry
                       Wait       -- ^ Error is temporary - wait and retry
                       deriving (Eq, Read, Show)

-- | Stanza errors are accommodated with one of the error conditions listed
-- below.
data StanzaErrorCondition = BadRequest            -- ^ Malformed XML.
                          | Conflict              -- ^ Resource or session with
                                                  --   name already exists.
                          | FeatureNotImplemented
                          | Forbidden             -- ^ Insufficient permissions.
                          | Gone (Maybe NonemptyText) -- ^ Entity can no longer
                                                      -- be contacted at this
                                                      -- address.
                          | InternalServerError
                          | ItemNotFound
                          | JidMalformed
                          | NotAcceptable         -- ^ Does not meet policy
                                                  --   criteria.
                          | NotAllowed            -- ^ No entity may perform
                                                  --   this action.
                          | NotAuthorized         -- ^ Must provide proper
                                                  --   credentials.
                          | PolicyViolation       -- ^ The entity has violated
                                                  -- some local service policy
                                                  -- (e.g., a message contains
                                                  -- words that are prohibited
                                                  -- by the service)
                          | RecipientUnavailable  -- ^ Temporarily unavailable.
                          | Redirect (Maybe NonemptyText) -- ^ Redirecting to
                                                          -- other entity,
                                                          -- usually
                                                          -- temporarily.
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
                            deriving (Eq, Read, Show)

-- =============================================================================
--  OTHER STUFF
-- =============================================================================

data SaslFailure = SaslFailure { saslFailureCondition :: SaslError
                               , saslFailureText :: Maybe ( Maybe LangTag
                                                          , Text
                                                          )
                               } deriving (Eq, Show)

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
               deriving (Eq, Read, Show)

-- The documentation of StreamErrorConditions is copied from
-- http://xmpp.org/rfcs/rfc6120.html#streams-error-conditions
data StreamErrorCondition
    = StreamBadFormat -- ^ The entity has sent XML that cannot be processed.
    | StreamBadNamespacePrefix -- ^ The entity has sent a namespace prefix that
                               -- is unsupported, or has sent no namespace
                               -- prefix on an element that needs such a prefix
    | StreamConflict -- ^ The server either (1) is closing the existing stream
                     -- for this entity because a new stream has been initiated
                     -- that conflicts with the existing stream, or (2) is
                     -- refusing a new stream for this entity because allowing
                     -- the new stream would conflict with an existing stream
                     -- (e.g., because the server allows only a certain number
                     -- of connections from the same IP address or allows only
                     -- one server-to-server stream for a given domain pair as a
                     -- way of helping to ensure in-order processing
    | StreamConnectionTimeout -- ^ One party is closing the stream because it
                              -- has reason to believe that the other party has
                              -- permanently lost the ability to communicate
                              -- over the stream.
    | StreamHostGone -- ^ The value of the 'to' attribute provided in the
                     -- initial stream header corresponds to an FQDN that is no
                     -- longer serviced by the receiving entity
    | StreamHostUnknown -- ^ The value of the 'to' attribute provided in the
                        -- initial stream header does not correspond to an FQDN
                        -- that is serviced by the receiving entity.
    | StreamImproperAddressing -- ^ A stanza sent between two servers lacks a
                               -- 'to' or 'from' attribute, the 'from' or 'to'
                               -- attribute has no value, or the value violates
                               -- the rules for XMPP addresses
    | StreamInternalServerError -- ^ The server has experienced a
                                -- misconfiguration or other internal error that
                                -- prevents it from servicing the stream.
    | StreamInvalidFrom -- ^ The data provided in a 'from' attribute does not
                        -- match an authorized JID or validated domain as
                        -- negotiated (1) between two servers using SASL or
                        -- Server Dialback, or (2) between a client and a server
                        -- via SASL authentication and resource binding.
    | StreamInvalidNamespace -- ^ The stream namespace name is something other
                             -- than \"http://etherx.jabber.org/streams\" (see
                             -- Section 11.2) or the content namespace declared
                             -- as the default namespace is not supported (e.g.,
                             -- something other than \"jabber:client\" or
                             -- \"jabber:server\").
    | StreamInvalidXml -- ^ The entity has sent invalid XML over the stream to a
                       -- server that performs validation
    | StreamNotAuthorized -- ^ The entity has attempted to send XML stanzas or
                          -- other outbound data before the stream has been
                          -- authenticated, or otherwise is not authorized to
                          -- perform an action related to stream negotiation;
                          -- the receiving entity MUST NOT process the offending
                          -- data before sending the stream error.
    | StreamNotWellFormed -- ^ The initiating entity has sent XML that violates
                          -- the well-formedness rules of [XML] or [XML‑NAMES].
    | StreamPolicyViolation -- ^ The entity has violated some local service
                            -- policy (e.g., a stanza exceeds a configured size
                            -- limit); the server MAY choose to specify the
                            -- policy in the \<text/\> element or in an
                            -- application-specific condition element.
    | StreamRemoteConnectionFailed -- ^ The server is unable to properly connect
                                   -- to a remote entity that is needed for
                                   -- authentication or authorization (e.g., in
                                   -- certain scenarios related to Server
                                   -- Dialback [XEP‑0220]); this condition is
                                   -- not to be used when the cause of the error
                                   -- is within the administrative domain of the
                                   -- XMPP service provider, in which case the
                                   -- \<internal-server-error /\> condition is
                                   -- more appropriate.
    | StreamReset -- ^ The server is closing the stream because it has new
                  -- (typically security-critical) features to offer, because
                  -- the keys or certificates used to establish a secure context
                  -- for the stream have expired or have been revoked during the
                  -- life of the stream , because the TLS sequence number has
                  -- wrapped, etc. The reset applies to the stream and to any
                  -- security context established for that stream (e.g., via TLS
                  -- and SASL), which means that encryption and authentication
                  -- need to be negotiated again for the new stream (e.g., TLS
                  -- session resumption cannot be used)
    | StreamResourceConstraint -- ^ The server lacks the system resources
                               -- necessary to service the stream.
    | StreamRestrictedXml -- ^ he entity has attempted to send restricted XML
                          -- features such as a comment, processing instruction,
                          -- DTD subset, or XML entity reference
    | StreamSeeOtherHost -- ^ The server will not provide service to the
                         -- initiating entity but is redirecting traffic to
                         -- another host under the administrative control of the
                         -- same service provider.
    | StreamSystemShutdown -- ^ The server is being shut down and all active
                           -- streams are being closed.
    | StreamUndefinedCondition -- ^ The error condition is not one of those
                               -- defined by the other conditions in this list
    | StreamUnsupportedEncoding -- ^ The initiating entity has encoded the
                                -- stream in an encoding that is not supported
                                -- by the server or has otherwise improperly
                                -- encoded the stream (e.g., by violating the
                                -- rules of the [UTF‑8] encoding).
    | StreamUnsupportedFeature -- ^ The receiving entity has advertised a
                               -- mandatory-to-negotiate stream feature that the
                               -- initiating entity does not support, and has
                               -- offered no other mandatory-to-negotiate
                               -- feature alongside the unsupported feature.
    | StreamUnsupportedStanzaType -- ^ The initiating entity has sent a
                                  -- first-level child of the stream that is not
                                  -- supported by the server, either because the
                                  -- receiving entity does not understand the
                                  -- namespace or because the receiving entity
                                  -- does not understand the element name for
                                  -- the applicable namespace (which might be
                                  -- the content namespace declared as the
                                  -- default namespace)
    | StreamUnsupportedVersion -- ^ The 'version' attribute provided by the
                               -- initiating entity in the stream header
                               -- specifies a version of XMPP that is not
                               -- supported by the server.
      deriving (Eq, Read, Show)

-- | Encapsulates information about an XMPP stream error.
data StreamErrorInfo = StreamErrorInfo
    { errorCondition :: !StreamErrorCondition
    , errorText      :: !(Maybe (Maybe LangTag, NonemptyText))
    , errorXml       :: !(Maybe Element)
    } deriving (Show, Eq)

data XmppTlsError = XmppTlsError TLSError
                  | XmppTlsException TLSException
                    deriving (Show, Eq, Typeable)

-- | Signals an XMPP stream error or another unpredicted stream-related
-- situation. This error is fatal, and closes the XMPP stream.
data XmppFailure = StreamErrorFailure StreamErrorInfo -- ^ An error XML stream
                                                        -- element has been
                                                        -- encountered.
                 | StreamEndFailure -- ^ The stream has been closed.
                                    -- This exception is caught by the
                                    -- concurrent implementation, and
                                    -- will thus not be visible
                                    -- through use of 'Session'.
                 | StreamCloseError ([Element], XmppFailure) -- ^ When an XmppFailure
                                              -- is encountered in
                                              -- closeStreams, this
                                              -- constructor wraps the
                                              -- elements collected so
                                              -- far.
                 | TcpConnectionFailure -- ^ All attempts to TCP
                                        -- connect to the server
                                        -- failed.
                 | XmppIllegalTcpDetails -- ^ The TCP details provided did not
                                         -- validate.
                 | TlsError XmppTlsError -- ^ An error occurred in the
                                     -- TLS layer
                 | TlsNoServerSupport -- ^ The server does not support
                                      -- the use of TLS
                 | XmppNoStream -- ^ An action that required an active
                                -- stream were performed when the
                                -- 'StreamState' was 'Closed'
                 | XmppAuthFailure AuthFailure -- ^ Authentication with the
                                               -- server failed (unrecoverably)
                 | TlsStreamSecured -- ^ Connection already secured
                 | XmppOtherFailure -- ^ Undefined condition. More
                                    -- information should be available in
                                    -- the log.
                 | XmppIOException IOException -- ^ An 'IOException'
                                               -- occurred
                 | XmppInvalidXml String -- ^ Received data is not valid XML
                 deriving (Show, Eq, Typeable)

instance Exception XmppFailure

-- | Signals a SASL authentication error condition.
data AuthFailure = -- | No mechanism offered by the server was matched
                   -- by the provided acceptable mechanisms; wraps the
                   -- mechanisms offered by the server
                   AuthNoAcceptableMechanism [Text.Text]
                 | AuthStreamFailure XmppFailure -- TODO: Remove
                   -- | A SASL failure element was encountered
                 | AuthSaslFailure SaslFailure
                   -- | The credentials provided did not conform to
                   -- the SASLprep Stringprep profile
                 | AuthIllegalCredentials
                   -- | Other failure; more information is available
                   -- in the log
                 | AuthOtherFailure
                 deriving (Eq, Show)

-- =============================================================================
--  XML TYPES
-- =============================================================================

-- | XMPP version number. Displayed as "\<major\>.\<minor\>". 2.4 is lesser than
-- 2.13, which in turn is lesser than 12.3.

data Version = Version { majorVersion :: !Integer
                       , minorVersion :: !Integer } deriving (Eq, Read, Show)

-- If the major version numbers are not equal, compare them. Otherwise, compare
-- the minor version numbers.
instance Ord Version where
    compare (Version amajor aminor) (Version bmajor bminor)
        | amajor /= bmajor = compare amajor bmajor
        | otherwise = compare aminor bminor

-- instance Read Version where
--     readsPrec _ txt = (,"") <$> maybeToList (versionFromText $ Text.pack txt)

-- instance Show Version where
--     show (Version major minor) = (show major) ++ "." ++ (show minor)

-- Converts a "<major>.<minor>" numeric version number to a @Version@ object.
versionFromText :: Text.Text -> Maybe Version
versionFromText s = case AP.parseOnly versionParser s of
    Right version -> Just version
    Left _ -> Nothing

-- Read numbers, a dot, more numbers, and end-of-file.
versionParser :: AP.Parser Version
versionParser = do
    major <- AP.many1 AP.digit
    AP.skip (== '.')
    minor <- AP.many1 AP.digit
    AP.endOfInput
    return $ Version (read major) (read minor)

-- | The language tag in accordance with RFC 5646 (in the form of "en-US"). It
-- has a primary tag and a number of subtags. Two language tags are considered
-- equal if and only if they contain the same tags (case-insensitive).
data LangTag = LangTag { primaryTag :: !Text
                       , subtags    :: ![Text] }

-- Equals for language tags is not case-sensitive.
instance Eq LangTag where
    LangTag p s == LangTag q t = Text.toLower p == Text.toLower q &&
        map Text.toLower s == map Text.toLower t

-- | Parses, validates, and possibly constructs a "LangTag" object.
langTagFromText :: Text.Text -> Maybe LangTag
langTagFromText s = case AP.parseOnly langTagParser s of
                        Right tag -> Just tag
                        Left _ -> Nothing

langTagToText :: LangTag -> Text.Text
langTagToText (LangTag p []) = p
langTagToText (LangTag p s) = Text.concat $ [p, "-", Text.intercalate "-" s]

-- Parses a language tag as defined by RFC 1766 and constructs a LangTag object.
langTagParser :: AP.Parser LangTag
langTagParser = do
    -- Read until we reach a '-' character, or EOF. This is the `primary tag'.
    primTag <- tag
    -- Read zero or more subtags.
    subTags <- many subtag
    AP.endOfInput
    return $ LangTag primTag subTags
  where
    tag :: AP.Parser Text.Text
    tag = do
        t <- AP.takeWhile1 $ AP.inClass tagChars
        return t
    subtag :: AP.Parser Text.Text
    subtag = do
        AP.skip (== '-')
        tag
    tagChars :: [Char]
    tagChars = ['a'..'z'] ++ ['A'..'Z']

data StreamFeatures = StreamFeatures
    { streamFeaturesTls         :: !(Maybe Bool)
    , streamFeaturesMechanisms  :: ![Text.Text]
    , streamFeaturesRosterVer   :: !(Maybe Bool)
      -- ^ @Nothing@ for no roster versioning, @Just False@ for roster
      -- versioning and @Just True@ when the server sends the non-standard
      -- "optional" element (observed with prosody).
    , streamFeaturesPreApproval :: !Bool -- ^ Does the server support pre-approval
    , streamFeaturesSession     :: !(Maybe Bool)
       -- ^ Does this server allow the stream elelemt? (See
       -- https://tools.ietf.org/html/draft-cridland-xmpp-session-01)
    , streamFeaturesOther       :: ![Element]
      -- TODO: All feature elements instead?
    } deriving (Eq, Show)

instance Sem.Semigroup StreamFeatures where
    sf1 <> sf2 =
        StreamFeatures
               { streamFeaturesTls = mplusOn streamFeaturesTls
               , streamFeaturesMechanisms  = mplusOn streamFeaturesMechanisms
               , streamFeaturesRosterVer   = mplusOn streamFeaturesRosterVer
               , streamFeaturesPreApproval =
                 streamFeaturesPreApproval sf1
                 || streamFeaturesPreApproval sf2
               , streamFeaturesSession   = mplusOn streamFeaturesSession
               , streamFeaturesOther       = mplusOn streamFeaturesOther

               }
      where
        mplusOn f = f sf1 `mplus` f sf2

instance Monoid StreamFeatures where
    mempty = StreamFeatures
               { streamFeaturesTls         = Nothing
               , streamFeaturesMechanisms  = []
               , streamFeaturesRosterVer   = Nothing
               , streamFeaturesPreApproval = False
               , streamFeaturesSession     = Nothing
               , streamFeaturesOther       = []
               }
    mappend = (<>)

-- | Signals the state of the stream connection.
data ConnectionState
    = Closed  -- ^ Stream has not been established yet
    | Plain   -- ^ Stream established, but not secured via TLS
    | Secured -- ^ Stream established and secured via TLS
    | Finished -- ^ Stream was closed
      deriving (Show, Eq, Typeable)

-- | Defines operations for sending, receiving, flushing, and closing on a
-- stream.
data StreamHandle =
    StreamHandle { streamSend :: BS.ByteString
                                 -> IO (Either XmppFailure ()) -- ^ Sends may not
                                                          -- interleave
                 , streamReceive :: Int -> IO (Either XmppFailure BS.ByteString)
                   -- This is to hold the state of the XML parser (otherwise we
                   -- will receive EventBeginDocument events and forget about
                   -- name prefixes). (TODO: Clarify)
                 , streamFlush :: IO ()
                 , streamClose :: IO ()
                 }

data StreamState = StreamState
    { -- | State of the stream - 'Closed', 'Plain', or 'Secured'
      streamConnectionState :: !ConnectionState
      -- | Functions to send, receive, flush, and close the stream
    , streamHandle :: StreamHandle
      -- | Event conduit source, and its associated finalizer
    , streamEventSource :: Source (ExceptT XmppFailure IO) Event
      -- | Stream features advertised by the server
    , streamFeatures :: !StreamFeatures -- TODO: Maybe?
      -- | The hostname or IP specified for the connection
    , streamAddress :: !(Maybe Text)
      -- | The hostname specified in the server's stream element's
      -- `from' attribute
    , streamFrom :: !(Maybe Jid)
      -- | The identifier specified in the server's stream element's
      -- `id' attribute
    , streamId :: !(Maybe Text)
      -- | The language tag value specified in the server's stream
      -- element's `langtag' attribute; will be a `Just' value once
      -- connected to the server
      -- TODO: Verify
    , streamLang :: !(Maybe LangTag)
      -- | Our JID as assigned by the server
    , streamJid :: !(Maybe Jid)
      -- | Configuration settings for the stream
    , streamConfiguration :: StreamConfiguration
    }

newtype Stream = Stream { unStream :: TMVar StreamState }

---------------
-- JID
---------------

-- | A JID is XMPP\'s native format for addressing entities in the network. It
-- is somewhat similar to an e-mail address but contains three parts instead of
-- two: localpart, domainpart, and resourcepart.
--
-- The @localpart@ of a JID is an optional identifier placed
-- before the domainpart and separated from the latter by a
-- \'\@\' character. Typically a localpart uniquely identifies
-- the entity requesting and using network access provided by a
-- server (i.e., a local account), although it can also
-- represent other kinds of entities (e.g., a chat room
-- associated with a multi-user chat service). The entity
-- represented by an XMPP localpart is addressed within the
-- context of a specific domain (i.e.,
-- @localpart\@domainpart@).
--
-- The domainpart typically identifies the /home/ server to
-- which clients connect for XML routing and data management
-- functionality. However, it is not necessary for an XMPP
-- domainpart to identify an entity that provides core XMPP
-- server functionality (e.g., a domainpart can identify an
-- entity such as a multi-user chat service, a
-- publish-subscribe service, or a user directory).
--
-- The resourcepart of a JID is an optional identifier placed
-- after the domainpart and separated from the latter by the
-- \'\/\' character. A resourcepart can modify either a
-- @localpart\@domainpart@ address or a mere @domainpart@
-- address. Typically a resourcepart uniquely identifies a
-- specific connection (e.g., a device or location) or object
-- (e.g., an occupant in a multi-user chat room) belonging to
-- the entity associated with an XMPP localpart at a domain
-- (i.e., @localpart\@domainpart/resourcepart@).
--
-- For more details see RFC 6122 <http://xmpp.org/rfcs/rfc6122.html>

data Jid = Jid { localpart_    :: !(Maybe NonemptyText)
               , domainpart_   :: !NonemptyText
               , resourcepart_ :: !(Maybe NonemptyText)
               } deriving (Eq, Ord)

-- | Converts a JID to a Text.
jidToText :: Jid -> Text
jidToText (Jid nd dmn res) = Text.concat . concat $
                             [ maybe [] (:["@"]) (text <$> nd)
                             , [text dmn]
                             , maybe [] (\r -> ["/",r]) (text <$> res)
                             ]

-- | Converts a JID to up to three Text values: (the optional) localpart, the
-- domainpart, and (the optional) resourcepart.
--
-- >>> jidToTexts [jid|foo@bar/quux|]
-- (Just "foo","bar",Just "quux")
--
-- >>> jidToTexts [jid|bar/quux|]
-- (Nothing,"bar",Just "quux")
--
-- >>> jidToTexts [jid|foo@bar|]
-- (Just "foo","bar",Nothing)
--
-- prop> jidToTexts j == (localpart j, domainpart j, resourcepart j)
jidToTexts :: Jid -> (Maybe Text, Text, Maybe Text)
jidToTexts (Jid nd dmn res) = (text <$> nd, text dmn, text <$> res)

-- Produces a Jid value in the format "parseJid \"<jid>\"".
instance Show Jid where
  show j = "parseJid " ++ show (jidToText j)

-- The string must be in the format "parseJid \"<jid>\"".
-- TODO: This function should produce its error values in a uniform way.
-- TODO: Do we need to care about precedence here?
instance Read Jid where
    readsPrec _ s = do
        -- Verifies that the first word is "parseJid", parses the second word and
        -- the remainder, if any, and produces these two values or fails.
      case lex s of
        [("parseJid", r')] ->
          case lex r' of
            [(s', r'')] ->
              case (reads s') of
                ((jidTxt,_):_) ->
                  case jidFromText (Text.pack jidTxt) of
                       Nothing -> []
                       Just jid' -> [(jid', r'')]
                _ -> []
            _ -> []
        _ -> []


#if WITH_TEMPLATE_HASKELL

instance TH.Lift Jid where
    lift (Jid lp dp rp) = [| Jid $(mbTextE $ text <$> lp)
                                 $(textE   $ text dp)
                                 $(mbTextE $ text <$> rp)
                           |]
     where
        textE t = [| Nonempty $ Text.pack $(stringE $ Text.unpack t) |]
        mbTextE Nothing = [| Nothing |]
        mbTextE (Just s) = [| Just $(textE s) |]

-- | Constructs and validates a @Jid@ at compile time.
--
-- Syntax:
-- @
--     [jid|localpart\@domainpart/resourcepart|]
-- @
--
-- >>> [jid|foo@bar/quux|]
-- parseJid "foo@bar/quux"
--
-- >>> Just [jid|foo@bar/quux|] == jidFromTexts (Just "foo") "bar" (Just "quux")
-- True
--
-- >>> Just [jid|foo@bar/quux|] == jidFromText "foo@bar/quux"
-- True
--
-- See also 'jidFromText'
jid :: QuasiQuoter
jid = QuasiQuoter { quoteExp = \s -> do
                          when (head s == ' ') . fail $ "Leading whitespaces in JID" ++ show s
                          let t = Text.pack s
                          when (Text.last t == ' ') . reportWarning $ "Trailing whitespace in JID " ++ show s
                          case jidFromText t of
                              Nothing -> fail $ "Could not parse JID " ++ s
                              Just j -> TH.lift j
                  , quotePat = error "Jid patterns aren't implemented"
                  , quoteType = error "jid QQ can't be used in type context"
                  , quoteDec  = error "jid QQ can't be used in declaration context"
                  }

-- | Synonym for 'jid'
jidQ :: QuasiQuoter
jidQ = jidQ
#endif

-- | The partial order of "definiteness". JID1 is less than or equal JID2 iff
-- the domain parts are equal and JID1's local part and resource part each are
-- either Nothing or equal to Jid2's
(<~) :: Jid -> Jid -> Bool
(Jid lp1 dp1 rp1) <~ (Jid lp2 dp2 rp2) =
    dp1 ==  dp2 &&
    lp1 ~<~ lp2 &&
    rp1 ~<~ rp2
  where
   Nothing  ~<~ _ = True
   Just x  ~<~ Just y = x == y
   _  ~<~ _ = False

-- Produces a LangTag value in the format "parseLangTag \"<jid>\"".
instance Show LangTag where
  show l = "parseLangTag " ++ show (langTagToText l)

-- The string must be in the format "parseLangTag \"<LangTag>\"". This is based
-- on parseJid, and suffers the same problems.
instance Read LangTag where
    readsPrec _ s = do
        let (s', r) = case lex s of
                          [] -> error "Expected `parseLangTag \"<LangTag>\"'"
                          [("parseLangTag", r')] -> case lex r' of
                                              [] -> error "Expected `parseLangTag \"<LangTag>\"'"
                                              [(s'', r'')] -> (s'', r'')
                                              _ -> error "Expected `parseLangTag \"<LangTag>\"'"
                          _ -> error "Expected `parseLangTag \"<LangTag>\"'"
        [(parseLangTag (read s' :: String), r)]

parseLangTag :: String -> LangTag
parseLangTag s = case langTagFromText $ Text.pack s of
                     Just l -> l
                     Nothing -> error $ "Language tag value (" ++ s ++ ") did not validate"

#if WITH_TEMPLATE_HASKELL
langTagQ :: QuasiQuoter
langTagQ = QuasiQuoter {quoteExp = \s -> case langTagFromText $ Text.pack  s of
                             Nothing -> fail $ "Not a valid language tag: "
                                               ++  s
                             Just lt -> [|LangTag $(textE $ primaryTag lt)
                                                  $(listE $
                                                      map textE (subtags lt))
                                        |]

                       , quotePat = error $ "LanguageTag patterns aren't"
                                         ++ " implemented"
                       , quoteType = error $ "LanguageTag QQ can't be used"
                                           ++ " in type context"
                       , quoteDec  = error $ "LanguageTag QQ can't be used"
                                           ++ " in declaration context"

                       }
  where
    textE t = [| Text.pack $(stringE $ Text.unpack t) |]
#endif
-- | Parses a JID string.
--
-- Note: This function is only meant to be used to reverse @Jid@ Show
-- operations; it will produce an 'undefined' value if the JID does not
-- validate; please refer to @jidFromText@ for a safe equivalent.
parseJid :: String -> Jid
parseJid s = case jidFromText $ Text.pack s of
                 Just j -> j
                 Nothing -> error $ "Jid value (" ++ s ++ ") did not validate"

-- | Parse a JID
--
-- >>> localpart <$> jidFromText "foo@bar/quux"
-- Just (Just "foo")
--
-- >>> domainpart <$> jidFromText "foo@bar/quux"
-- Just "bar"
--
-- >>> resourcepart <$> jidFromText "foo@bar/quux"
-- Just (Just "quux")
--
-- @ and / can occur in the domain part
--
-- >>> jidFromText "foo/bar@quux/foo"
-- Just parseJid "foo/bar@quux/foo"
--
-- * Counterexamples
--
-- A JID must only have one \'\@\':
--
-- >>> jidFromText "foo@bar@quux"
-- Nothing
--
-- The domain part can\'t be empty:
--
-- >>> jidFromText "foo@/quux"
-- Nothing
--
-- Both the local part and the resource part can be omitted (but the
-- \'\@\' and \'\/\', must also be removed):
--
-- >>> jidToTexts <$> jidFromText "bar"
-- Just (Nothing,"bar",Nothing)
--
-- >>> jidToTexts <$> jidFromText "@bar"
-- Nothing
--
-- >>> jidToTexts <$> jidFromText "bar/"
-- Nothing
--
jidFromText :: Text -> Maybe Jid
jidFromText t = do
    (l, d, r) <- eitherToMaybe $ AP.parseOnly jidParts t
    jidFromTexts l d r
  where
    eitherToMaybe = either (const Nothing) Just

-- | Convert localpart, domainpart, and resourcepart to a JID. Runs the
-- appropriate stringprep profiles and validates the parts.
--
-- >>> jidFromTexts (Just "foo") "bar" (Just "baz") == jidFromText "foo@bar/baz"
-- True
--
-- prop> \j -> jidFromTexts (localpart j) (domainpart j) (resourcepart j) == Just j
jidFromTexts :: Maybe Text -> Text -> Maybe Text -> Maybe Jid
jidFromTexts l d r = do
    localPart <- case l of
        Nothing -> return Nothing
        Just l'-> do
            l'' <- SP.runStringPrep nodeprepProfile l'
            guard $ validPartLength l''
            let prohibMap = Set.fromList nodeprepExtraProhibitedCharacters
            guard $ Text.all (`Set.notMember` prohibMap) l''
            l''' <- nonEmpty l''
            return $ Just l'''
    domainPart' <- SP.runStringPrep (SP.namePrepProfile False) (stripSuffix d)
    guard $ validDomainPart domainPart'
    guard $ validPartLength domainPart'
    domainPart <- nonEmpty domainPart'
    resourcePart <- case r of
        Nothing -> return Nothing
        Just r' -> do
            r'' <- SP.runStringPrep resourceprepProfile r'
            guard $ validPartLength r''
            r''' <- nonEmpty r''
            return $ Just r'''
    return $ Jid localPart domainPart resourcePart
  where
    validDomainPart :: Text -> Bool
    validDomainPart s = not $ Text.null s -- TODO: implement more stringent
                                          -- checks

    validPartLength :: Text -> Bool
    validPartLength p = Text.length p > 0
                        && BS.length (Text.encodeUtf8 p) < 1024
    -- RFC6122 §2.2
    stripSuffix t = if Text.last t == '.' then Text.init t else t

-- | Returns 'True' if the JID is /bare/, that is, it doesn't have a resource
-- part, and 'False' otherwise.
--
-- >>> isBare [jid|foo@bar|]
-- True
--
-- >>> isBare [jid|foo@bar/quux|]
-- False
isBare :: Jid -> Bool
isBare j | resourcepart j == Nothing = True
         | otherwise                 = False

-- | Returns 'True' if the JID is /full/, and 'False' otherwise.
--
-- @isFull = not . isBare@
--
-- >>> isBare [jid|foo@bar|]
-- True
--
-- >>> isBare [jid|foo@bar/quux|]
-- False
isFull :: Jid -> Bool
isFull = not . isBare

-- | Returns the @Jid@ without the resourcepart (if any).
--
-- >>> toBare [jid|foo@bar/quux|] == [jid|foo@bar|]
-- True
toBare :: Jid -> Jid
toBare j  = j{resourcepart_ = Nothing}

-- | Returns the localpart of the @Jid@ (if any).
--
-- >>> localpart [jid|foo@bar/quux|]
-- Just "foo"
localpart :: Jid -> Maybe Text
localpart = fmap text . localpart_

-- | Returns the domainpart of the @Jid@.
--
-- >>> domainpart [jid|foo@bar/quux|]
-- "bar"
domainpart :: Jid -> Text
domainpart = text . domainpart_

-- | Returns the resourcepart of the @Jid@ (if any).
--
-- >>> resourcepart [jid|foo@bar/quux|]
-- Just "quux"
resourcepart :: Jid -> Maybe Text
resourcepart = fmap text . resourcepart_

-- | Parse the parts of a JID. The parts need to be validated with stringprep
-- before the JID can be constructed
jidParts :: AP.Parser (Maybe Text, Text, Maybe Text)
jidParts = do
    maybeLocalPart <- Just <$> localPart <|> return Nothing
    domainPart <- AP.takeWhile1 (AP.notInClass ['@', '/'])
    maybeResourcePart <- Just <$> resourcePart <|> return Nothing
    AP.endOfInput
    return (maybeLocalPart, domainPart, maybeResourcePart)
  where
    localPart = do
        bytes <- AP.takeWhile1 (AP.notInClass ['@', '/'])
        _ <- AP.char '@'
        return bytes
    resourcePart = do
        _ <- AP.char '/'
        AP.takeText



-- | The `nodeprep' StringPrep profile.
nodeprepProfile :: SP.StringPrepProfile
nodeprepProfile = SP.Profile { SP.maps = [SP.b1, SP.b2]
                             , SP.shouldNormalize = True
                             , SP.prohibited = [ SP.a1
                                               , SP.c11
                                               , SP.c12
                                               , SP.c21
                                               , SP.c22
                                               , SP.c3
                                               , SP.c4
                                               , SP.c5
                                               , SP.c6
                                               , SP.c7
                                               , SP.c8
                                               , SP.c9
                                               ]
                             , SP.shouldCheckBidi = True
                             }

-- | These characters needs to be checked for after normalization.
nodeprepExtraProhibitedCharacters :: [Char]
nodeprepExtraProhibitedCharacters = ['\x22', '\x26', '\x27', '\x2F', '\x3A',
                                     '\x3C', '\x3E', '\x40']

-- | The `resourceprep' StringPrep profile.
resourceprepProfile :: SP.StringPrepProfile
resourceprepProfile = SP.Profile { SP.maps = [SP.b1]
                                 , SP.shouldNormalize = True
                                 , SP.prohibited = [ SP.a1
                                                   , SP.c12
                                                   , SP.c21
                                                   , SP.c22
                                                   , SP.c3
                                                   , SP.c4
                                                   , SP.c5
                                                   , SP.c6
                                                   , SP.c7
                                                   , SP.c8
                                                   , SP.c9
                                                   ]
                                 , SP.shouldCheckBidi = True
                                 }
-- | Specify the method with which the connection is (re-)established
data ConnectionDetails = UseRealm -- ^ Use realm to resolv host. This is the
                                  -- default.
                       | UseSrv HostName -- ^ Use this hostname for a SRV lookup
                       | UseHost HostName PortNumber -- ^ Use specified host
                       | UseConnection (ExceptT XmppFailure IO StreamHandle)
                         -- ^ Use a custom method to create a StreamHandle. This
                         -- will also be used by reconnect. For example, to
                         -- establish TLS before starting the stream as done by
                         -- GCM, see 'connectTls'. You can also return an
                         -- already established connection. This method should
                         -- also return a hostname that is used for TLS
                         -- signature verification. If startTLS is not used it
                         -- can be left empty

-- | Configuration settings related to the stream.
data StreamConfiguration =
    StreamConfiguration { -- | Default language when no language tag is set
                          preferredLang :: !(Maybe LangTag)
                          -- | JID to include in the stream element's `to'
                          -- attribute when the connection is secured; if the
                          -- boolean is set to 'True', then the JID is also
                          -- included when the 'ConnectionState' is 'Plain'
                        , toJid :: !(Maybe (Jid, Bool))
                          -- | By settings this field, clients can specify the
                          -- network interface to use, override the SRV lookup
                          -- of the realm, as well as specify the use of a
                          -- non-standard port when connecting by IP or
                          -- connecting to a domain without SRV records.
                        , connectionDetails :: ConnectionDetails
                          -- | DNS resolver configuration
                        , resolvConf :: ResolvConf
                          -- | Whether or not to perform the legacy
                          -- session bind as defined in the (outdated)
                          -- RFC 3921 specification
                        , tlsBehaviour :: TlsBehaviour
                          -- | Settings to be used for TLS negotitation
                        , tlsParams :: ClientParams
                        }

-- | Default parameters for TLS restricted to strong ciphers
xmppDefaultParamsStrong :: ClientParams
xmppDefaultParamsStrong = (defaultParamsClient "" BS.empty)
                        { clientSupported = def
                            { supportedCiphers = ciphersuite_strong
                                                 ++ [ cipher_AES256_SHA1
                                                    , cipher_AES128_SHA1
                                                    ]
                            }
                        }

-- | Default parameters for TLS
xmppDefaultParams :: ClientParams
xmppDefaultParams = (defaultParamsClient "" BS.empty)
                        { clientSupported = def
                            { supportedCiphers = ciphersuite_all
                            }
                        }

instance Default StreamConfiguration where
    def = StreamConfiguration { preferredLang     = Nothing
                              , toJid             = Nothing
                              , connectionDetails = UseRealm
                              , resolvConf        = defaultResolvConf
                              , tlsBehaviour      = PreferTls
                              , tlsParams         = xmppDefaultParams
                              }

-- | How the client should behave in regards to TLS.
data TlsBehaviour = RequireTls -- ^ Require the use of TLS; disconnect if it's
                               -- not offered.
                  | PreferTls  -- ^ Negotitate TLS if it's available.
                  | PreferPlain  -- ^ Negotitate TLS only if the server requires
                                 -- it
                  | RefuseTls  -- ^ Never secure the stream with TLS.
