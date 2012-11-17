-- |
-- Module:      $Header$
-- Description: A work in progress client implementation of RFC 6120 (XMPP:
--              Core).
-- License:     Apache License 2.0
--
-- Maintainer:  jon.kristensen@nejla.com
-- Stability:   unstable
-- Portability: portable
--
-- The Extensible Messaging and Presence Protocol (XMPP) is an open technology
-- for near-real-time communication, which powers a wide range of applications
-- including instant messaging, presence, multi-party chat, voice and video
-- calls, collaboration, lightweight middleware, content syndication, and
-- generalized routing of XML data. XMPP provides a technology for the
-- asynchronous, end-to-end exchange of structured data by means of direct,
-- persistent XML streams among a distributed network of globally addressable,
-- presence-aware clients and servers.
--
-- Pontarius is an XMPP client library, implementing the core capabilities of
-- XMPP (RFC 6120): setup and teardown of XML streams, channel encryption,
-- authentication, error handling, and communication primitives for messaging.
--
-- Note that we are not recommending anyone to use Pontarius XMPP at this time
-- as it's still in an experimental stage and will have its API and data types
-- modified frequently.

{-# LANGUAGE NoMonomorphismRestriction, OverloadedStrings #-}

module Network.Xmpp
  ( -- * Session management
    newSessionChans
  , withConnection
  , connect
  , simpleConnect
  , startTLS
  , simpleAuth
  , auth
  , closeConnection
  , endSession
  , setConnectionClosedHandler
  -- * JID
  -- | A JID (historically: Jabber ID) is XMPPs native format
  -- for addressing entities in the network. It is somewhat similar to an e-mail
  -- address but contains three parts instead of two:
  , Jid(..)
  , isBare
  , isFull
  -- * Stanzas
  -- | The basic protocol data unit in XMPP is the XML stanza. The stanza is
  -- essentially a fragment of XML that is sent over a stream. @Stanzas@ come in
  -- 3 flavors:
  --
  --  * @'Message'@, for traditional push-style message passing between peers
  --
  --  * @'Presence'@, for communicating status updates
  --
  --  * IQ (info/query), for request-response semantics communication
  --
  -- All stanza types have the following attributes in common:
  --
  --  * The /id/ attribute is used by the originating entity to track any
  --    response or error stanza that it might receive in relation to the
  --    generated stanza from another entity (such as an intermediate server or
  --    the intended recipient).  It is up to the originating entity whether the
  --    value of the 'id' attribute is unique only within its current stream or
  --    unique globally.
  --
  --  * The /from/ attribute specifies the JID of the sender.
  --
  --  * The /to/ attribute specifies the JID of the intended recipient for the
  --    stanza.
  --
  --  * The /type/ attribute specifies the purpose or context of the message,
  --    presence, or IQ stanza. The particular allowable values for the 'type'
  --    attribute vary depending on whether the stanza is a message, presence,
  --    or IQ stanza.
  , getStanzaChan
  -- ** Messages
  -- | The /message/ stanza is a /push/ mechanism whereby one entity pushes
  -- information to another entity, similar to the communications that occur in
  -- a system such as email.
  --
  -- <http://xmpp.org/rfcs/rfc6120.html#stanzas-semantics-message>
  , Message(..)
  , MessageError(..)
  , MessageType(..)
  -- *** creating
  , answerMessage
  -- *** sending
  , sendMessage
  -- *** receiving
  , pullMessage
  , waitForMessage
  , waitForMessageError
  , filterMessages
  -- ** Presence
  -- | XMPP includes the ability for an entity to advertise its network
  -- availability, or "presence", to other entities. In XMPP, this availability
  -- for communication is signaled end-to-end by means of a dedicated
  -- communication primitive: the presence stanza.
  , Presence(..)
  , PresenceError(..)
  -- *** creating
  , module Network.Xmpp.Presence
  -- *** sending
  -- | Sends a presence stanza. In general, the presence stanza should have no
  -- 'to' attribute, in which case the server to which the client is connected
  -- will broadcast that stanza to all subscribed entities. However, a
  -- publishing client may also send a presence stanza with a 'to' attribute, in
  -- which case the server will route or deliver that stanza to the intended
  -- recipient.
  , sendPresence
  -- *** receiving
  , pullPresence
  , waitForPresence
  -- ** IQ
  -- | Info\/Query, or IQ, is a /request-response/ mechanism, similar in some
  -- ways to the Hypertext Transfer Protocol @HTTP@. The semantics of IQ enable
  -- an entity to make a request of, and receive a response from, another
  -- entity. The data content and precise semantics  of the request and response
  -- is defined by the schema or other structural definition associated with the
  -- XML namespace that qualifies the direct child element of the IQ element. IQ
  -- interactions follow a common pattern of structured data exchange such as
  -- get\/result or set\/result (although an error can be returned in reply to a
  -- request if appropriate)
  --
  -- <http://xmpp.org/rfcs/rfc6120.html#stanzas-semantics-iq>
  , IQRequest(..)
  , IQRequestTicket
  , iqRequestBody
  , IQRequestType(..)
  , IQResult(..)
  , IQError(..)
  , IQResponse(..)
  , sendIQ
  , sendIQ'
  , answerIQ
  , listenIQChan
  , iqRequestPayload
  , iqResultPayload
  -- * Threads
  , forkChans
  -- * Misc
  , LangTag(..)
  , exampleParams
  ) where

import           Data.Text as Text

import           Network
import qualified Network.TLS as TLS
import           Network.Xmpp.Bind
import           Network.Xmpp.Concurrent
import           Network.Xmpp.Concurrent.Channels
import           Network.Xmpp.Concurrent.Types
import           Network.Xmpp.Marshal
import           Network.Xmpp.Message
import           Network.Xmpp.Monad
import           Network.Xmpp.Pickle
import           Network.Xmpp.Presence
import           Network.Xmpp.Sasl
import           Network.Xmpp.Sasl.Mechanisms
import           Network.Xmpp.Sasl.Types
import           Network.Xmpp.Session
import           Network.Xmpp.Stream
import           Network.Xmpp.TLS
import           Network.Xmpp.Types

import           Control.Monad.Error

-- | Connect to host with given address.
connect :: HostName -> PortID -> Text -> XmppConMonad (Either StreamError ())
connect address port hostname = do
    xmppRawConnect address port hostname
    result <- xmppStartStream
    case result of
        Left e -> do
            pushElement . pickleElem xpStreamError $ toError e
            xmppCloseStreams
            return ()
        Right () -> return ()
    return result
  where
        -- TODO: Descriptive texts in stream errors?
        toError  (StreamNotStreamElement _name) =
                XmppStreamError StreamInvalidXml Nothing Nothing
        toError  (StreamInvalidStreamNamespace _ns) =
                XmppStreamError StreamInvalidNamespace Nothing Nothing
        toError  (StreamInvalidStreamPrefix _prefix) =
                XmppStreamError StreamBadNamespacePrefix Nothing Nothing
        -- TODO: Catch remaining xmppStartStream errors.
        toError  (StreamWrongVersion _ver) =
                XmppStreamError StreamUnsupportedVersion Nothing Nothing
        toError  (StreamWrongLangTag _) =
                XmppStreamError StreamInvalidXml Nothing Nothing
        toError  StreamUnknownError =
                XmppStreamError StreamBadFormat Nothing Nothing


-- | Authenticate to the server using the first matching method and bind a
-- resource.
auth :: [SaslHandler]
     -> Maybe Text
     -> XmppConMonad (Either AuthError Jid)
auth mechanisms resource = runErrorT $ do
    ErrorT $ xmppSasl mechanisms
    jid <- lift $ xmppBind resource
    lift $ xmppStartSession
    return jid

-- | Authenticate to the server with the given username and password
-- and bind a resource.
--
-- Prefers SCRAM-SHA1 over DIGEST-MD5.
simpleAuth  :: Text.Text  -- ^ The username
            -> Text.Text  -- ^ The password
            -> Maybe Text -- ^ The desired resource or 'Nothing' to let the
                          -- server assign one
            -> XmppConMonad (Either AuthError Jid)
simpleAuth username passwd resource = flip auth resource $
        [ -- TODO: scramSha1Plus
          scramSha1 username Nothing passwd
        , digestMd5 username Nothing passwd
        ]



-- | The quick and easy way to set up a connection to an XMPP server
--
-- This will
--   * connect to the host
--   * secure the connection with TLS
--   * authenticate to the server using either SCRAM-SHA1 (preferred) or
--     Digest-MD5
--   * bind a resource
--   * return the full JID you have been assigned
--
-- Note that the server might assign a different resource even when we send
-- a preference.
simpleConnect :: HostName   -- ^ Target host name
              -> PortID
              -> Text       -- ^ User name (authcid)
              -> Text       -- ^ Password
              -> Maybe Text -- ^ Desired resource (or Nothing to let the server
                            -- decide)
              -> XmppConMonad Jid
simpleConnect host port username password resource = do
      connect host port username
      startTLS exampleParams
      saslResponse <- simpleAuth username password resource
      case saslResponse of
          Right jid -> return jid
          Left e -> error $ show e
