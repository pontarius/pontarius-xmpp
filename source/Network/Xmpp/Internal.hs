-- |
-- Module:      $Header$
-- 
-- Maintainer:  info@jonkri.com
-- Stability:   unstable
-- Portability: portable
-- 
-- This module allows for low-level access to Pontarius XMPP. Generally, the
-- "Network.Xmpp" module should be used instead.
-- 
-- The 'Stream' object provides the most low-level access to the XMPP
-- stream: a simple and single-threaded interface which exposes the conduit
-- 'Event' source, as well as the input and output byte streams. Custom stateful
-- 'Stream' functions can be executed using 'withStream'.
-- 
-- The TLS, SASL, and 'Session' functionalities of Pontarius XMPP are built on
-- top of this API.

module Network.Xmpp.Internal
  ( Stream(..)
  , StreamState(..)
  , StreamHandle(..)
  , StreamFeatures(..)
  , openStream
  , withStream
  , startTls
  , auth
  , pushStanza
  , pullStanza
  , pushIQ
  , SaslHandler(..)
  , StanzaId(..)
 )

       where

import Network.Xmpp.Stream
import Network.Xmpp.Sasl
import Network.Xmpp.Sasl.Common
import Network.Xmpp.Sasl.Types
import Network.Xmpp.Tls
import Network.Xmpp.Types
import Network.Xmpp.Stream
import Network.Xmpp.Marshal
