-- Copyright © 2010-2012 Jon Kristensen. See the LICENSE file in the
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
-- XMPP is an open standard, extendable, and secure communications
-- protocol designed on top of XML, TLS, and SASL. Pontarius XMPP is
-- an XMPP client library, implementing the core capabilities of XMPP
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

module Network.XMPP ( -- Network.XMPP.JID
                      Address (..)
                      , Localpart
                      , Domainpart
                      , Resourcepart
                    , isFull
                    , isBare
                    , fromString
                    , fromStrings

                      -- Network.XMPP.Session
                    -- , ClientHandler (..)
                    -- , ClientState (..)
                    -- , ConnectResult (..)
                    -- , HostName
                    -- , Password
                    -- , PortNumber
                    -- , Resource
                    -- , Session
                    -- , TerminationReason
                    -- , UserName
                    -- , sendIQ
                    -- , sendPresence
                    -- , sendMessage
                    -- , connect
                    -- , openStreams
                    -- , tlsSecureStreams
                    -- , authenticate
                    -- , session
                    -- , OpenStreamResult (..)
                    -- , SecureWithTLSResult (..)
                    -- , AuthenticateResult (..)

                      -- Network.XMPP.Stanza
                    , StanzaID (SID)
                    , From
                    , To
                    , LangTag
                    , MessageType (..)
                    , Message (..)
                    , PresenceType (..)
                    , Presence (..)
                    , IQ (..)
                    , iqPayloadNamespace
                    , iqPayload ) where

import Network.XMPP.Address
import Network.XMPP.SASL
import Network.XMPP.Session
import Network.XMPP.Stanza
import Network.XMPP.Utilities
import Network.XMPP.Types
import Network.XMPP.TLS
import Network.XMPP.Stream

