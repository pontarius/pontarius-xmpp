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
    , ServerFeatures(..)
    , Stanza(..)
    , StanzaError(..)
    , StanzaErrorCondition(..)
    , StanzaErrorType(..)
    , StanzaId(..)
    , StreamError(..)
    , StreamErrorCondition(..)
    , Version(..)
    , XmppConMonad
    , XmppConnection(..)
    , XmppConnectionState(..)
    , XmppT(..)
    , XmppStreamError(..)
    , langTag
    , module Network.Xmpp.Jid
    )
       where

import           Control.Applicative ((<$>), many)
import           Control.Exception
import           Control.Monad.IO.Class
import           Control.Monad.State.Strict
import           Control.Monad.Error

import qualified Data.Attoparsec.Text as AP
import qualified Data.ByteString as BS
import           Data.Conduit
import           Data.String(IsString(..))
import           Data.Maybe (fromJust, fromMaybe, maybeToList)
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Typeable(Typeable)
import           Data.XML.Types

import qualified Network as N

import           Network.Xmpp.Jid

import           System.IO

-- |
-- Wraps a string of random characters that, when using an appropriate
-- @IDGenerator@, is guaranteed to be unique for the Xmpp session.

data StanzaId = SI !Text deriving (Eq, Ord)

instance Show StanzaId where
  show (SI s) = Text.unpack s

instance Read StanzaId where
  readsPrec _ x = [(SI $ Text.pack x, "")]

instance IsString StanzaId where
  fromString = SI . Text.pack

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
data IQRequest = IQRequest { iqRequestID      :: !StanzaId
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
data IQResult = IQResult { iqResultID      :: !StanzaId
                         , iqResultFrom    :: !(Maybe Jid)
                         , iqResultTo      :: !(Maybe Jid)
                         , iqResultLangTag :: !(Maybe LangTag)
                         , iqResultPayload :: !(Maybe Element)
                         } deriving Show

-- | The answer to an IQ request that generated an error.
data IQError = IQError { iqErrorID          :: !StanzaId
                       , iqErrorFrom        :: !(Maybe Jid)
                       , iqErrorTo          :: !(Maybe Jid)
                       , iqErrorLangTag     :: !(Maybe LangTag)
                       , iqErrorStanzaError :: !StanzaError
                       , iqErrorPayload     :: !(Maybe Element) -- should this be []?
                       } deriving Show

-- | The message stanza. Used for /push/ type communication.
data Message = Message { messageID      :: !(Maybe StanzaId)
                       , messageFrom    :: !(Maybe Jid)
                       , messageTo      :: !(Maybe Jid)
                       , messageLangTag :: !(Maybe LangTag)
                       , messageType    :: !MessageType
                       , messagePayload :: ![Element]
                       } deriving Show

-- | An error stanza generated in response to a 'Message'.
data MessageError = MessageError { messageErrorID          :: !(Maybe StanzaId)
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
data Presence = Presence { presenceID      :: !(Maybe StanzaId)
                         , presenceFrom    :: !(Maybe Jid)
                         , presenceTo      :: !(Maybe Jid)
                         , presenceLangTag :: !(Maybe LangTag)
                         , presenceType    :: !(Maybe PresenceType)
                         , presencePayload :: ![Element]
                         } deriving Show


-- | An error stanza generated in response to a 'Presence'.
data PresenceError = PresenceError { presenceErrorID          :: !(Maybe StanzaId)
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

data XmppStreamError = XmppStreamError
    { errorCondition :: !StreamErrorCondition
    , errorText      :: !(Maybe (Maybe LangTag, Text))
    , errorXML       :: !(Maybe Element)
    } deriving (Show, Eq)

data StreamError = StreamError XmppStreamError
                 | StreamUnknownError -- Something has gone wrong, but we don't
                                      -- know what
                 | StreamNotStreamElement Text
                 | StreamInvalidStreamNamespace (Maybe Text)
                 | StreamInvalidStreamPrefix (Maybe Text)
                 | StreamWrongTo (Maybe Text)
                 | StreamWrongVersion (Maybe Text)
                 | StreamWrongLangTag (Maybe Text)
                 | StreamXMLError String -- If stream pickling goes wrong.
                 | StreamStreamEnd -- received closing stream tag
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
        subtag <- tag
        return subtag
    tagChars :: [Char]
    tagChars = ['a'..'z'] ++ ['A'..'Z']

data ServerFeatures = SF
    { stls           :: !(Maybe Bool)
    , saslMechanisms :: ![Text.Text]
    , other          :: ![Element]
    } deriving Show

data XmppConnectionState
    = XmppConnectionClosed  -- ^ No connection at this point.
    | XmppConnectionPlain   -- ^ Connection established, but not secured.
    | XmppConnectionSecured -- ^ Connection established and secured via TLS.
      deriving (Show, Eq, Typeable)

data XmppConnection = XmppConnection
               { sConSrc          :: !(Source IO Event) -- ^ inbound connection
               , sRawSrc          :: !(Source IO BS.ByteString) -- ^ inbound
                                                                -- connection
               , sConPushBS       :: !(BS.ByteString -> IO Bool) -- ^ outbound
                                                                 -- connection
               , sConHandle       :: !(Maybe Handle) -- ^ Handle for TLS
               , sFeatures        :: !ServerFeatures -- ^ Features the server
                                                     -- advertised
               , sConnectionState :: !XmppConnectionState -- ^ State of connection
               , sHostname        :: !(Maybe Text) -- ^ Hostname of the server
               , sJid             :: !(Maybe Jid) -- ^ Our JID
               , sCloseConnection :: !(IO ()) -- ^ necessary steps to cleanly
                                              -- close the connection (send TLS
                                              -- bye etc.)
               , sPreferredLang   :: !(Maybe LangTag) -- ^ Default language when
                                                      -- no explicit language
                                                      -- tag is set
               , sStreamLang      :: !(Maybe LangTag) -- ^ Will be a `Just' value
                                                    -- once connected to the
                                                    -- server.
               , sStreamId        :: !(Maybe Text) -- ^ Stream ID as specified by
                                                   -- the server.
               , sToJid           :: !(Maybe Jid) -- ^ JID to include in the
                                                  -- stream element's `to'
                                                  -- attribute when the
                                                  -- connection is secured. See
                                                  -- also below.
               , sJidWhenPlain    :: !Bool -- ^ Whether or not to also include the
                                           -- Jid when the connection is plain.
               , sFrom            :: !(Maybe Jid)  -- ^ From as specified by the
                                                   -- server in the stream
                                                   -- element's `from'
                                                   -- attribute.
               }

-- |
-- The Xmpp monad transformer. Contains internal state in order to
-- work with Pontarius. Pontarius clients needs to operate in this
-- context.
newtype XmppT m a = XmppT { runXmppT :: StateT XmppConnection m a } deriving (Monad, MonadIO)

-- | Low-level and single-threaded Xmpp monad. See @Xmpp@ for a concurrent
-- implementation.
type XmppConMonad a = StateT XmppConnection IO a

-- Make XmppT derive the Monad and MonadIO instances.
deriving instance (Monad m, MonadIO m) => MonadState (XmppConnection) (XmppT m)
