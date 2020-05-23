-- |
-- Module:      $Header$
--
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
  ( module Network.Xmpp.Concurrent
  , module Network.Xmpp.Concurrent.Basic
  , module Network.Xmpp.Concurrent.IQ
  , module Network.Xmpp.Concurrent.Message
  , module Network.Xmpp.Concurrent.Monad
  , module Network.Xmpp.Concurrent.Presence
  , module Network.Xmpp.Concurrent.Threads
  , module Network.Xmpp.Concurrent.Types
  , module Network.Xmpp.IM.Message
  , module Network.Xmpp.IM.Presence
  , module Network.Xmpp.IM.Roster
  , module Network.Xmpp.IM.Roster.Types
  , module Network.Xmpp.Marshal
  , module Network.Xmpp.Sasl
  , module Network.Xmpp.Sasl.Common
  , module Network.Xmpp.Sasl.Mechanisms
  , module Network.Xmpp.Sasl.Mechanisms.DigestMd5
  , module Network.Xmpp.Sasl.Mechanisms.Plain
  , module Network.Xmpp.Sasl.Mechanisms.Scram
  , module Network.Xmpp.Sasl.StringPrep
  , module Network.Xmpp.Sasl.Types
  , module Network.Xmpp.Stanza
  , module Network.Xmpp.Stream
  , module Network.Xmpp.Tls
  , module Network.Xmpp.Types
  , module Network.Xmpp.Utilities
  ) where


import Network.Xmpp.Concurrent
import Network.Xmpp.Concurrent.Basic
import Network.Xmpp.Concurrent.IQ
import Network.Xmpp.Concurrent.Message
import Network.Xmpp.Concurrent.Monad
import Network.Xmpp.Concurrent.Presence
import Network.Xmpp.Concurrent.Threads
import Network.Xmpp.Concurrent.Types
import Network.Xmpp.IM.Message
import Network.Xmpp.IM.Presence
import Network.Xmpp.IM.Roster
import Network.Xmpp.IM.Roster.Types
import Network.Xmpp.Marshal
import Network.Xmpp.Sasl
import Network.Xmpp.Sasl.Common
import Network.Xmpp.Sasl.Mechanisms
import Network.Xmpp.Sasl.Mechanisms.DigestMd5
import Network.Xmpp.Sasl.Mechanisms.Plain
import Network.Xmpp.Sasl.Mechanisms.Scram
import Network.Xmpp.Sasl.StringPrep
import Network.Xmpp.Sasl.Types
import Network.Xmpp.Stanza
import Network.Xmpp.Stream hiding (mbl, lmb)
import Network.Xmpp.Tls
import Network.Xmpp.Types
import Network.Xmpp.Utilities
