{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_HADDOCK hide #-}

module Network.Xmpp.Types
    ( IQError(..)
    , IQRequest(..)
    , IQRequestType(..)
    , IQResponse(..)
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
    , StreamFeatures(..)
    , Stanza(..)
    , StanzaError(..)
    , StanzaErrorCondition(..)
    , StanzaErrorType(..)
    , StanzaID(..)
    , XmppFailure(..)
    , StreamErrorCondition(..)
    , Version(..)
    , StreamHandle(..)
    , Stream(..)
    , StreamState(..)
    , ConnectionState(..)
    , StreamErrorInfo(..)
    , StanzaHandler
    , ConnectionDetails(..)
    , StreamConfiguration(..)
    , langTag
    , Jid(..)
    , isBare
    , isFull
    , jidFromText
    , jidFromTexts
    , StreamEnd(..)
    , InvalidXmppXml(..)
    , SessionConfiguration(..)
    , TlsBehaviour(..)
    )
       where

import           Control.Applicative ((<$>), (<|>), many)
import           Control.Concurrent.STM
import           Control.Exception
import           Control.Monad.Error
import qualified Data.Attoparsec.Text as AP
import qualified Data.ByteString as BS
import           Data.Conduit
import           Data.Default
import           Data.Maybe (fromJust, maybeToList)
import qualified Data.Set as Set
import           Data.String (IsString(..))
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Typeable(Typeable)
import           Data.XML.Types
import           Network
import           Network.DNS
import           Network.TLS hiding (Version)
import           Network.TLS.Extra
import qualified Text.NamePrep as SP
import qualified Text.StringPrep as SP

-- |
-- Wraps a string of random characters that, when using an appropriate
-- @IdGenerator@, is guaranteed to be unique for the Xmpp session.

data StanzaID = StanzaID !Text deriving (Eq, Ord)

instance Show StanzaID where
  show (StanzaID s) = Text.unpack s

instance Read StanzaID where
  readsPrec _ x = [(StanzaID $ Text.pack x, "")]

instance IsString StanzaID where
  fromString = StanzaID . Text.pack

-- | The Xmpp communication primities (Message, Presence and Info/Query) are
-- called stanzas.
data Stanza = IQRequestS     !IQRequest
            | IQResultS      !IQResult
            | IQErrorS       !IQError
            | MessageS       !Message
            | MessageErrorS  !MessageError
            | PresenceS      !Presence
            | PresenceErrorS !PresenceError
              deriving Show

-- | A "request" Info/Query (IQ) stanza is one with either "get" or "set" as
-- type. It always contains an xml payload.
data IQRequest = IQRequest { iqRequestID      :: !StanzaID
                           , iqRequestFrom    :: !(Maybe Jid)
                           , iqRequestTo      :: !(Maybe Jid)
                           , iqRequestLangTag :: !(Maybe LangTag)
                           , iqRequestType    :: !IQRequestType
                           , iqRequestPayload :: !Element
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

-- | A "response" Info/Query (IQ) stanza is either an 'IQError', an IQ stanza
-- of  type "result" ('IQResult') or a Timeout.
data IQResponse = IQResponseError IQError
                | IQResponseResult IQResult
                | IQResponseTimeout
                deriving Show

-- | The (non-error) answer to an IQ request.
data IQResult = IQResult { iqResultID      :: !StanzaID
                         , iqResultFrom    :: !(Maybe Jid)
                         , iqResultTo      :: !(Maybe Jid)
                         , iqResultLangTag :: !(Maybe LangTag)
                         , iqResultPayload :: !(Maybe Element)
                         } deriving Show

-- | The answer to an IQ request that generated an error.
data IQError = IQError { iqErrorID          :: !StanzaID
                       , iqErrorFrom        :: !(Maybe Jid)
                       , iqErrorTo          :: !(Maybe Jid)
                       , iqErrorLangTag     :: !(Maybe LangTag)
                       , iqErrorStanzaError :: !StanzaError
                       , iqErrorPayload     :: !(Maybe Element) -- should this be []?
                       } deriving Show

-- | The message stanza. Used for /push/ type communication.
data Message = Message { messageID      :: !(Maybe StanzaID)
                       , messageFrom    :: !(Maybe Jid)
                       , messageTo      :: !(Maybe Jid)
                       , messageLangTag :: !(Maybe LangTag)
                       , messageType    :: !MessageType
                       , messagePayload :: ![Element]
                       } deriving Show

-- | An error stanza generated in response to a 'Message'.
data MessageError = MessageError { messageErrorID          :: !(Maybe StanzaID)
                                 , messageErrorFrom        :: !(Maybe Jid)
                                 , messageErrorTo          :: !(Maybe Jid)
                                 , messageErrorLangTag     :: !(Maybe LangTag)
                                 , messageErrorStanzaError :: !StanzaError
                                 , messageErrorPayload     :: ![Element]
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
    show Chat      = "chat"
    show GroupChat = "groupchat"
    show Headline  = "headline"
    show Normal    = "normal"

instance Read MessageType where
    readsPrec _  "chat"      = [(Chat, "")]
    readsPrec _  "groupchat" = [(GroupChat, "")]
    readsPrec _  "headline"  = [(Headline, "")]
    readsPrec _  "normal"    = [(Normal, "")]
    readsPrec _  _           = [(Normal, "")]

-- | The presence stanza. Used for communicating status updates.
data Presence = Presence { presenceID      :: !(Maybe StanzaID)
                         , presenceFrom    :: !(Maybe Jid)
                         , presenceTo      :: !(Maybe Jid)
                         , presenceLangTag :: !(Maybe LangTag)
                         , presenceType    :: !(Maybe PresenceType)
                         , presencePayload :: ![Element]
                         } deriving Show


-- | An error stanza generated in response to a 'Presence'.
data PresenceError = PresenceError { presenceErrorID          :: !(Maybe StanzaID)
                                   , presenceErrorFrom        :: !(Maybe Jid)
                                   , presenceErrorTo          :: !(Maybe Jid)
                                   , presenceErrorLangTag     :: !(Maybe LangTag)
                                   , presenceErrorStanzaError :: !StanzaError
                                   , presenceErrorPayload     :: ![Element]
                                   } deriving Show

-- | @PresenceType@ holds Xmpp presence types. The "error" message type is left
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

-- | All stanzas (IQ, message, presence) can cause errors, which in the Xmpp
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
    show Cancel   = "cancel"
    show Continue = "continue"
    show Modify   = "modify"
    show Auth     = "auth"
    show Wait     = "wait"

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
                          | JidMalformed
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
    show JidMalformed = "jid-malformed"
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
    readsPrec _  "jid-malformed"           = [(JidMalformed         , "")]
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
                             -- than "http://etherx.jabber.org/streams" (see
                             -- Section 11.2) or the content namespace declared
                             -- as the default namespace is not supported (e.g.,
                             -- something other than "jabber:client" or
                             -- "jabber:server").
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
                                   -- <internal-server-error/> condition is more
                                   -- appropriate.
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

-- | Encapsulates information about an XMPP stream error.
data StreamErrorInfo = StreamErrorInfo
    { errorCondition :: !StreamErrorCondition
    , errorText      :: !(Maybe (Maybe LangTag, Text))
    , errorXml       :: !(Maybe Element)
    } deriving (Show, Eq)

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
                 | TlsError TLSError -- ^ An error occurred in the
                                     -- TLS layer
                 | TlsNoServerSupport -- ^ The server does not support
                                      -- the use of TLS
                 | XmppNoStream -- ^ An action that required an active
                                -- stream were performed when the
                                -- 'StreamState' was 'Closed'
                 | XmppAuthFailure -- ^ Authentication with the server failed
                                   -- unrecoverably
                 | TlsStreamSecured -- ^ Connection already secured
                 | XmppOtherFailure -- ^ Undefined condition. More
                                    -- information should be available in
                                    -- the log.
                 | XmppIOException IOException -- ^ An 'IOException'
                                               -- occurred
                 deriving (Show, Eq, Typeable)

instance Exception XmppFailure
instance Error XmppFailure where noMsg = XmppOtherFailure

-- =============================================================================
--  XML TYPES
-- =============================================================================


-- | Wraps a function that MUST generate a stream of unique Ids. The
--   strings MUST be appropriate for use in the stanza id attirubte.
--   For a default implementation, see @idGenerator@.

newtype IdGenerator = IdGenerator (IO Text)


-- | XMPP version number. Displayed as "<major>.<minor>". 2.4 is lesser than
-- 2.13, which in turn is lesser than 12.3.

data Version = Version { majorVersion :: !Integer
                       , minorVersion :: !Integer } deriving (Eq)

-- If the major version numbers are not equal, compare them. Otherwise, compare
-- the minor version numbers.
instance Ord Version where
    compare (Version amajor aminor) (Version bmajor bminor)
        | amajor /= bmajor = compare amajor bmajor
        | otherwise = compare aminor bminor

instance Read Version where
    readsPrec _ txt = (,"") <$> maybeToList (versionFromText $ Text.pack txt)

instance Show Version where
    show (Version major minor) = (show major) ++ "." ++ (show minor)

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

instance Eq LangTag where
    LangTag p s == LangTag q t = Text.toLower p == Text.toLower q &&
        map Text.toLower s == map Text.toLower t

instance Read LangTag where
    readsPrec _ txt = (,"") <$> maybeToList (langTag $ Text.pack txt)

instance Show LangTag where
    show (LangTag p []) = Text.unpack p
    show (LangTag p s) = Text.unpack . Text.concat $
        [p, "-", Text.intercalate "-" s]

-- | Parses, validates, and possibly constructs a "LangTag" object.
langTag :: Text.Text -> Maybe LangTag
langTag s = case AP.parseOnly langTagParser s of
              Right tag -> Just tag
              Left _ -> Nothing

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
    { streamTls            :: !(Maybe Bool)
    , streamSaslMechanisms :: ![Text.Text]
    , streamOtherFeatures  :: ![Element] -- TODO: All feature elements instead?
    } deriving Show

-- | Signals the state of the stream connection.
data ConnectionState
    = Closed  -- ^ No stream has been established
    | Plain   -- ^ Stream established, but not secured via TLS
    | Secured -- ^ Stream established and secured via TLS
      deriving (Show, Eq, Typeable)

-- | Defines operations for sending, receiving, flushing, and closing on a
-- stream.
data StreamHandle =
    StreamHandle { streamSend :: BS.ByteString -> IO Bool
                 , streamReceive :: Int -> IO BS.ByteString
                   -- This is to hold the state of the XML parser (otherwise we
                   -- will receive EventBeginDocument events and forget about
                   -- name prefixes). (TODO: Clarify)
                 , streamFlush :: IO ()
                 , streamClose :: IO ()
                 }

data StreamState = StreamState
    { -- | State of the stream - 'Closed', 'Plain', or 'Secured'
      streamConnectionState :: !ConnectionState -- ^ State of connection
      -- | Functions to send, receive, flush, and close on the stream
    , streamHandle :: StreamHandle
      -- | Event conduit source, and its associated finalizer
    , streamEventSource :: Source IO Event
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
-- two.
data Jid = Jid { -- | The @localpart@ of a JID is an optional identifier placed
                 -- before the domainpart and separated from the latter by a
                 -- \'\@\' character. Typically a localpart uniquely identifies
                 -- the entity requesting and using network access provided by a
                 -- server (i.e., a local account), although it can also
                 -- represent other kinds of entities (e.g., a chat room
                 -- associated with a multi-user chat service). The entity
                 -- represented by an XMPP localpart is addressed within the
                 -- context of a specific domain (i.e.,
                 -- @localpart\@domainpart@).
                 localpart :: !(Maybe Text)

                 -- | The domainpart typically identifies the /home/ server to
                 -- which clients connect for XML routing and data management
                 -- functionality. However, it is not necessary for an XMPP
                 -- domainpart to identify an entity that provides core XMPP
                 -- server functionality (e.g., a domainpart can identify an
                 -- entity such as a multi-user chat service, a
                 -- publish-subscribe service, or a user directory).
               , domainpart :: !Text

                 -- | The resourcepart of a JID is an optional identifier placed
                 -- after the domainpart and separated from the latter by the
                 -- \'\/\' character. A resourcepart can modify either a
                 -- @localpart\@domainpart@ address or a mere @domainpart@
                 -- address. Typically a resourcepart uniquely identifies a
                 -- specific connection (e.g., a device or location) or object
                 -- (e.g., an occupant in a multi-user chat room) belonging to
                 -- the entity associated with an XMPP localpart at a domain
                 -- (i.e., @localpart\@domainpart/resourcepart@).
               , resourcepart :: !(Maybe Text)
               } deriving (Eq, Ord)

instance Show Jid where
  show (Jid nd dmn res) =
      maybe "" ((++ "@") . Text.unpack) nd ++ Text.unpack dmn ++
          maybe "" (('/' :) . Text.unpack) res

instance Read Jid where
  readsPrec _ x = case jidFromText (Text.pack x) of
      Nothing -> []
      Just j -> [(j,"")]

instance IsString Jid where
  fromString = fromJust . jidFromText . Text.pack

-- | Converts a Text to a JID.
jidFromText :: Text -> Maybe Jid
jidFromText t = do
    (l, d, r) <- eitherToMaybe $ AP.parseOnly jidParts t
    jidFromTexts l d r
  where
    eitherToMaybe = either (const Nothing) Just

-- | Converts localpart, domainpart, and resourcepart strings to a JID. Runs the
-- appropriate stringprep profiles and validates the parts.
jidFromTexts :: Maybe Text -> Text -> Maybe Text -> Maybe Jid
jidFromTexts l d r = do
    localPart <- case l of
        Nothing -> return Nothing
        Just l'-> do
            l'' <- SP.runStringPrep nodeprepProfile l'
            guard $ validPartLength l''
            let prohibMap = Set.fromList nodeprepExtraProhibitedCharacters
            guard $ Text.all (`Set.notMember` prohibMap) l''
            return $ Just l''
    domainPart <- SP.runStringPrep (SP.namePrepProfile False) d
    guard $ validDomainPart domainPart
    resourcePart <- case r of
        Nothing -> return Nothing
        Just r' -> do
            r'' <- SP.runStringPrep resourceprepProfile r'
            guard $ validPartLength r''
            return $ Just r''
    return $ Jid localPart domainPart resourcePart
  where
    validDomainPart :: Text -> Bool
    validDomainPart _s = True -- TODO

    validPartLength :: Text -> Bool
    validPartLength p = Text.length p > 0 && Text.length p < 1024

-- | Returns 'True' if the JID is /bare/, and 'False' otherwise.
isBare :: Jid -> Bool
isBare j | resourcepart j == Nothing = True
         | otherwise                 = False

-- | Returns 'True' if the JID is /full/, and 'False' otherwise.
isFull :: Jid -> Bool
isFull = not . isBare

-- Parses an JID string and returns its three parts. It performs no validation
-- or transformations.
jidParts :: AP.Parser (Maybe Text, Text, Maybe Text)
jidParts = do
    -- Read until we reach an '@', a '/', or EOF.
    a <- AP.takeWhile1 (AP.notInClass ['@', '/'])
    -- Case 1: We found an '@', and thus the localpart. At least the domainpart
    -- is remaining. Read the '@' and until a '/' or EOF.
    do
        b <- domainPartP
        -- Case 1A: We found a '/' and thus have all the JID parts. Read the '/'
        -- and until EOF.
        do
            c <- resourcePartP -- Parse resourcepart
            return (Just a, b, Just c)
        -- Case 1B: We have reached EOF; the JID is in the form
        -- localpart@domainpart.
            <|> do
                AP.endOfInput
                return (Just a, b, Nothing)
          -- Case 2: We found a '/'; the JID is in the form
          -- domainpart/resourcepart.
          <|> do
              b' <- resourcePartP
              AP.endOfInput
              return (Nothing, a, Just b')
          -- Case 3: We have reached EOF; we have an JID consisting of only a
          -- domainpart.
        <|> do
            AP.endOfInput
            return (Nothing, a, Nothing)
  where
    -- Read an '@' and everything until a '/'.
    domainPartP :: AP.Parser Text
    domainPartP = do
        _ <- AP.char '@'
        AP.takeWhile1 (/= '/')
    -- Read everything until a '/'.
    resourcePartP :: AP.Parser Text
    resourcePartP = do
        _ <- AP.char '/'
        AP.takeText

-- The `nodeprep' StringPrep profile.
nodeprepProfile :: SP.StringPrepProfile
nodeprepProfile = SP.Profile { SP.maps = [SP.b1, SP.b2]
                             , SP.shouldNormalize = True
                             , SP.prohibited = [SP.a1
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

-- These characters needs to be checked for after normalization.
nodeprepExtraProhibitedCharacters :: [Char]
nodeprepExtraProhibitedCharacters = ['\x22', '\x26', '\x27', '\x2F', '\x3A',
                                     '\x3C', '\x3E', '\x40']

-- The `resourceprep' StringPrep profile.
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

data StreamEnd = StreamEnd deriving (Typeable, Show)
instance Exception StreamEnd

data InvalidXmppXml = InvalidXmppXml String deriving (Show, Typeable)

instance Exception InvalidXmppXml

data ConnectionDetails = UseRealm -- ^ Use realm to resolv host
                       | UseSrv HostName -- ^ Use this hostname for a SRC lookup
                       | UseHost HostName PortID -- ^ Use specified host

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
                        , establishSession :: Bool
                          -- | How the client should behave in regards to TLS.
                        , tlsBehaviour :: TlsBehaviour
                          -- | Settings to be used for TLS negotitation
                        , tlsParams :: TLSParams
                        }

instance Default StreamConfiguration where
    def = StreamConfiguration { preferredLang = Nothing
                              , toJid = Nothing
                              , connectionDetails = UseRealm
                              , resolvConf = defaultResolvConf
                              , establishSession = True
                              , tlsBehaviour = PreferTls
                              , tlsParams = defaultParamsClient { pConnectVersion = TLS10
                                                                , pAllowedVersions = [TLS10, TLS11, TLS12]
                                                                , pCiphers = ciphersuite_strong
                                                                }
                              }

type StanzaHandler =  TChan Stanza -- ^ outgoing stanza
                   -> Stanza       -- ^ stanza to handle
                   -> IO Bool      -- ^ True when processing should continue

-- | Configuration for the @Session@ object.
data SessionConfiguration = SessionConfiguration
    { -- | Configuration for the @Stream@ object.
      sessionStreamConfiguration :: StreamConfiguration
      -- | Handler to be run when the session ends (for whatever reason).
    , sessionClosedHandler       :: XmppFailure -> IO ()
      -- | Function to generate the stream of stanza identifiers.
    , sessionStanzaIDs           :: IO (IO StanzaID)
    , extraStanzaHandlers        :: [StanzaHandler]
    , enableRoster               :: Bool
    }

instance Default SessionConfiguration where
    def = SessionConfiguration { sessionStreamConfiguration = def
                               , sessionClosedHandler = \_ -> return ()
                               , sessionStanzaIDs = do
                                     idRef <- newTVarIO 1
                                     return . atomically $ do
                                         curId <- readTVar idRef
                                         writeTVar idRef (curId + 1 :: Integer)
                                         return . StanzaID . Text.pack . show $ curId
                               , extraStanzaHandlers = []
                               , enableRoster = True
                               }

-- | How the client should behave in regards to TLS.
data TlsBehaviour = RequireTls -- ^ Require the use of TLS; disconnect if it's
                               -- not offered.
                  | PreferTls  -- ^ Negotitate TLS if it's available.
                  | PreferPlain  -- ^ Negotitate TLS only if the server requires
                                 -- it
                  | RefuseTls  -- ^ Never secure the stream with TLS.
