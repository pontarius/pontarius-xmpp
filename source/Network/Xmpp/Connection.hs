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
-- The 'Connection' object provides the most low-level access to the XMPP
-- stream: a simple and single-threaded interface which exposes the conduit
-- 'Event' source, as well as the input and output byte streams. Custom stateful
-- 'Connection' functions can be executed using 'withConnection'.
-- 
-- The TLS, SASL, and 'Session' functionalities of Pontarius XMPP are built on
-- top of this API.

module Network.Xmpp.Connection
  ( Connection(..)
  , ConnectionState(..)
  , ConnectionHandle(..)
  , ServerFeatures(..)
  , connect
  , withConnection
  , startTls
  , simpleAuth
  , auth
  , pushStanza
  , pullStanza
  , closeConnection
  , newSession
 )

       where

import Network.Xmpp.Connection_
import Network.Xmpp.Sasl
import Network.Xmpp.Session
import Network.Xmpp.Stream
import Network.Xmpp.Tls
import Network.Xmpp.Types
import Network.Xmpp.Concurrent