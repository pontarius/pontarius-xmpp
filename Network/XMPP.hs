{-

Copyright © 2010-2011 Jon Kristensen.

This file is part of Pontarius XMPP.

Pontarius XMPP is free software: you can redistribute it and/or modify it under
the terms of the GNU Lesser General Public License as published by the Free
Software Foundation, either version 3 of the License, or (at your option) any
later version.

Pontarius XMPP is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public License for more
details.

You should have received a copy of the GNU Lesser General Public License along
with Pontarius XMPP. If not, see <http://www.gnu.org/licenses/>.

-}

-- | Module:      $Header$
--   Description: A minimalistic and easy-to-use XMPP library
--   Copyright:   Copyright © 2010-2011 Jon Kristensen
--   License:     LGPL-3
--
--   Maintainer:  info@pontarius.org
--   Stability:   unstable
--   Portability: portable

--   Pontarius XMPP aims to be a secure, concurrent/event-based and easy-to-use
--   XMPP library for Haskell. It is being actively developed.
--
--   Note that we are not recommending anyone to use Pontarius XMPP at this time
--   as it's still in an experimental stage and will have its API and data types
--   modified frequently. See the project's web site at
--   <http://www.pontarius.org/> for more information.
--
--   This module will be documented soon.

module Network.XMPP ( -- Network.XMPP.JID
                      Address (..)
                      , Localpart
                      , Serverpart
                      , Resourcepart
                    , isFull
                    , isBare
                    , fromString
                    , fromStrings

                      -- Network.XMPP.SASL
                    , replyToChallenge1

                      -- Network.XMPP.Session
                    , Certificate
                    , ClientHandler (..)
                    , ClientState (..)
                    , ConnectResult (..)
                    , HostName
                    , Password
                    , PortNumber
                    , Resource
                    , Session
                    , TerminationReason
                    , UserName
                    , sendIQ
                    , sendPresence
                    , sendMessage
                    , connect
                    , openStream
                    , secureWithTLS
                    , authenticate
                    , session
                    , OpenStreamResult (..)
                    , SecureWithTLSResult (..)
                    , AuthenticateResult (..)

                      -- Network.XMPP.Stanza
                    , StanzaID (SID)
                    , From
                    , To
                    , XMLLang
                    , MessageType (..)
                    , Message (..)
                    , PresenceType (..)
                    , Presence (..)
                    , IQ (..)
                    , iqPayloadNamespace
                    , iqPayload

                    , injectAction

                    -- Network.XMPP.Utilities
                    , elementToString
                    , elementsToString
                    , getID ) where

import Network.XMPP.Address
import Network.XMPP.SASL
import Network.XMPP.Session
import Network.XMPP.Stanza
import Network.XMPP.Utilities
import Network.XMPP.Types
import Network.XMPP.TLS
import Network.XMPP.Stream

