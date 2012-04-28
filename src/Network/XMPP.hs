-- Copyright © 2010-2012 Jon Kristensen.
-- Copyright 2012 Philipp Balzarek
-- See the LICENSE file in the
-- Pontarius distribution for more details.

-- |
-- Module:      $Header$
-- Description: Pontarius API
-- Copyright:   Copyright © 2010-2012 Jon Kristensen
-- License:     Apache License 2.0
--
-- Maintainer:  jon.kristensen@nejla.com
-- Stability:   unstable
-- Portability: portable
--
-- The Extensible Messaging and Presence Protocol (XMPP) is an open technology for
-- real-time communication, which powers a wide range of applications including
-- instant messaging, presence, multi-party chat, voice and video calls,
-- collaboration, lightweight middleware, content syndication, and generalized
-- routing of XML data.
-- Pontarius an XMPP client library, implementing the core capabilities of XMPP
-- (RFC 6120).
--
-- Developers using this library are assumed to understand how XMPP
-- works.
--
-- This module will be documented soon.
--
-- Note that we are not recommending anyone to use Pontarius XMPP at
-- this time as it's still in an experimental stage and will have its
-- API and data types modified frequently.

{-# LANGUAGE NoMonomorphismRestriction, OverloadedStrings  #-}

module Network.XMPP
  ( -- * Session management
    withNewSession
  , withSession
  , newSession
  , connect
  , startTLS
  , auth
  , endSession
  , setSessionEndHandler
  -- * JID
  -- | A JID (historically: Jabber ID) is XMPPs native format
  -- for addressing entities in the network. It is somewhat similar to an
  -- email-address but contains three parts instead of two:
  , JID(..)
  -- * Stanzas
  -- | @Stanzas@ are the the smallest unit of communication in @XMPP@. They
  -- come in 3 flavors:
  --
  --  * @'Message'@, for traditional IM-style message passing between peers
  --
  --  * @'Presence'@, for communicating status updates
  --
  --  * IQ (info/query), with a request-response semantics
  --
  -- All stanza types have the following attributes in common:
  --
  --  * The /id/ attribute is used by the originating entity to track
  --    any response or error stanza that it might receive in relation to
  --    the generated stanza from another entity (such as an intermediate
  --    server or the intended recipient).  It is up to the originating
  --    entity whether the value of the 'id' attribute is unique only
  --    within its current stream or unique globally.
  --
  --  * The /from/ attribute specifies the JID of the sender.
  --
  --  * The /to/ attribute specifies the JID of the intended recipient
  --  for the stanza.
  --
  --  * The /type/ attribute specifies the purpose or context of the
  --    message, presence, or IQ stanza. The particular allowable values
  --    for the 'type' attribute vary depending on whether the stanza is
  --    a message, presence, or IQ stanza.

  -- ** Messages
  -- | The /message/ stanza is a /push/ mechanism whereby one entity pushes
  -- information to another entity, similar to the communications that occur in
  -- a system such as email.
  --
  -- <http://xmpp.org/rfcs/rfc6120.html#stanzas-semantics-message>
  , Message
  , MessageError
  -- *** creating
  , module Network.XMPP.Message
  -- *** sending
  , sendMessage
  -- *** receiving
  , pullMessage
  , waitForMessage
  , waitForMessageError
  , filterMessages
  -- ** Presence
  -- | The /presence/ stanza is a specialized /broadcast/
  -- or /publish-subscribe/ mechanism, whereby multiple entities
  -- receive information about an entity to which they have
  -- subscribed.
  --
  -- <http://xmpp.org/rfcs/rfc6120.html#stanzas-semantics-presence>
  , Presence(..)
  , PresenceError(..)
  , ShowType(..)
  -- *** creating
  , module Network.XMPP.Presence
  -- *** sending
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
  -- XML namespace that
  -- qualifies the direct child element of the IQ element. IQ interactions
  -- follow a common pattern of structured data
  -- exchange such as get/result or set/result (although an error can be returned
  -- in reply to a request if appropriate)
  --
  -- <http://xmpp.org/rfcs/rfc6120.html#stanzas-semantics-iq>
  , IQRequest(..)
  , IQRequestType(..)
  , IQResult(..)
  , IQError(..)
  , sendIQ
  , sendIQ'
  , answerIQ
  , listenIQChan
  , iqRequestPayload
  , iqResultPayload
  -- * Threads
  , XMPP
  , fork
  , forkSession
  -- * Misc
  , exampleParams
  ) where

import Data.Text as Text

import Network
import qualified Network.TLS as TLS
import Network.XMPP.Bind
import Network.XMPP.Concurrent
import Network.XMPP.Message hiding (message)
import Network.XMPP.Monad
import Network.XMPP.Presence hiding (presence)
import Network.XMPP.SASL
import Network.XMPP.Session
import Network.XMPP.Stream
import Network.XMPP.TLS
import Network.XMPP.Types

import Control.Monad.Error

-- | Connect to host with given address.
xmppConnect :: HostName -> Text -> XMPPConMonad (Either StreamError ())
xmppConnect  address hostname = xmppRawConnect address hostname >> xmppStartStream

-- | Attempts to secure the connection using TLS. Will return
-- 'TLSNoServerSupport' when the server does not offer TLS or does not
-- expect it at this time.
startTLS  :: TLS.TLSParams -> XMPP (Either XMPPTLSError ())
startTLS = withConnection . xmppStartTLS

-- | Authenticate to the server with the given username and password
-- and bind a resource
auth  :: Text.Text  -- ^ The username
      -> Text.Text  -- ^ The password
      -> Maybe Text -- ^ The desired resource or 'Nothing' to let the server
                    -- assign one
      -> XMPP (Either SaslError Text.Text)
auth username passwd resource = runErrorT $ do
    ErrorT . withConnection $ xmppSASL username passwd
    res <- lift $ xmppBind resource
    lift $ startSession
    return res

-- | Connect to an xmpp server
connect :: HostName -> Text -> XMPP (Either StreamError ())
connect address hostname = withConnection $ xmppConnect address hostname
