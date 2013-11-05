-- |
-- Module:      $Header$
--
-- Maintainer:  info@jonkri.com
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
-- Pontarius XMPP is an XMPP client library, implementing the core capabilities
-- of XMPP (RFC 6120): setup and teardown of XML streams, channel encryption,
-- authentication, error handling, and communication primitives for messaging.
--
-- For low-level access to Pontarius XMPP, see the "Network.Xmpp.Internal"
-- module.

{-# LANGUAGE CPP, NoMonomorphismRestriction, OverloadedStrings #-}

module Network.Xmpp
  ( -- * Session management
    Session
  , session
  , setConnectionClosedHandler
  , reconnect
  , reconnect'
  , reconnectNow
  , StreamConfiguration(..)
  , SessionConfiguration(..)
  , ConnectionDetails(..)
  , closeConnection
  , endSession
  , waitForStream
    -- TODO: Close session, etc.
    -- ** Authentication handlers
    -- | The use of 'scramSha1' is /recommended/, but 'digestMd5' might be
    -- useful for interaction with older implementations.
  , scramSha1
  , plain
  , digestMd5
  -- * Addressing
  -- | A JID (historically: Jabber ID) is XMPPs native format
  -- for addressing entities in the network. It is somewhat similar to an e-mail
  -- address, but contains three parts instead of two.
  , Jid
#if WITH_TEMPLATE_HASKELL
  , jidQ
#endif
  , isBare
  , isFull
  , jidFromText
  , jidFromTexts
  , jidToText
  , jidToTexts
  , toBare
  , localpart
  , domainpart
  , resourcepart
  , parseJid
  , getJid
  -- * Stanzas
  -- | The basic protocol data unit in XMPP is the XML stanza. The stanza is
  -- essentially a fragment of XML that is sent over a stream. @Stanzas@ come in
  -- 3 flavors:
  --
  --  * /Message/, for traditional push-style message passing between peers
  --
  --  * /Presence/, for communicating status updates
  --
  --  * /Info/\//Query/ (or /IQ/), for request-response semantics communication
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
  , getStanza
  , getStanzaChan
  , newStanzaID
  -- ** Messages
  -- | The /message/ stanza is a /push/ mechanism whereby one entity
  -- pushes information to another entity, similar to the communications that
  -- occur in a system such as email. It is not to be confused with
  -- /instant messaging/ which is handled in the 'Network.Xmpp.IM' module
  , Message(..)
  , message
  , MessageError(..)
  , MessageType(..)
  -- *** Creating
  , answerMessage
  -- *** Sending
  , sendMessage
  -- *** Receiving
  , pullMessage
  , getMessage
  , waitForMessage
  , waitForMessageError
  , filterMessages
  -- ** Presence
  -- | XMPP includes the ability for an entity to advertise its network
  -- availability, or "presence", to other entities. In XMPP, this availability
  -- for communication is signaled end-to-end by means of a dedicated
  -- communication primitive: the presence stanza.
  , Presence(..)
  , PresenceType(..)
  , PresenceError(..)
  -- *** Creating
  , presence
  , presenceOffline
  , presenceOnline
  , presenceSubscribe
  , presenceSubscribed
  , presenceUnsubscribe
  , presTo
  -- *** Sending
  -- | Sends a presence stanza. In general, the presence stanza should have no
  -- 'to' attribute, in which case the server to which the client is connected
  -- will broadcast that stanza to all subscribed entities. However, a
  -- publishing client may also send a presence stanza with a 'to' attribute, in
  -- which case the server will route or deliver that stanza to the intended
  -- recipient.
  , sendPresence
  -- *** Receiving
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
  , dropIQChan
  -- * Errors
  , StanzaError(..)
  , StanzaErrorType(..)
  , StanzaErrorCondition(..)
  , SaslFailure(..)
  -- * Lenses
  -- | You can import Network.Xmpp.Lens for basic lens functions ('view',
  -- 'modify' and 'set')

  -- ** Stanzas
  , HasStanzaAttrs(..)
  , HasStanzaID(..)
  , sid'
  , HasStanzaPayload(..)
  , HasStanzaError(..)
  , messageTypeL
  , presenceTypeL
  , iqRequestTypeL
  -- ** StanzaError
  , stanzaErrorTypeL
  , stanzaErrorConditionL
  , stanzaErrorTextL
  , stanzaErrorApplL
  -- ** StreamConfiguration
  , preferredLangL
  , toJidL
  , connectionDetailsL
  , resolvConfL
  , establishSessionL
  , tlsBehaviourL
  , tlsParamsL
  -- * Threads
  , dupSession
  -- * Miscellaneous
  , LangTag
  , langTagFromText
  , langTagToText
  , parseLangTag
  , XmppFailure(..)
  , StreamErrorInfo(..)
  , StreamErrorCondition(..)
  , AuthFailure( AuthNoAcceptableMechanism
               , AuthSaslFailure
               , AuthIllegalCredentials
               , AuthOtherFailure )
  , SaslHandler
  , ConnectionState(..)
  , connectTls
  ) where

import Network.Xmpp.Concurrent
import Network.Xmpp.Sasl
import Network.Xmpp.Sasl.Types
import Network.Xmpp.Stanza
import Network.Xmpp.Types
import Network.Xmpp.Tls
import Network.Xmpp.Lens
