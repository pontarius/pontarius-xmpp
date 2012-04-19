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

{-# LANGUAGE NoMonomorphismRestriction, OverloadedStrings  #-}

module Network.XMPP
  ( module Network.XMPP.Bind
  , module Network.XMPP.Concurrent
  , module Network.XMPP.Monad
  , module Network.XMPP.SASL
  , module Network.XMPP.Session
  , module Network.XMPP.Stream
  , module Network.XMPP.TLS
  , module Network.XMPP.Types
  , module Network.XMPP.Presence
  , module Network.XMPP.Message
  , xmppConnect
  , xmppNewSession
  , connect
  , startTLS
  , auth
  ) where

import Data.Text as Text

import Network
import qualified Network.TLS as TLS
import Network.XMPP.Bind
import Network.XMPP.Concurrent
import Network.XMPP.Message
import Network.XMPP.Monad
import Network.XMPP.Presence
import Network.XMPP.SASL
import Network.XMPP.Session
import Network.XMPP.Stream
import Network.XMPP.TLS
import Network.XMPP.Types

xmppConnect :: HostName -> Text -> XMPPConMonad (Either StreamError ())
xmppConnect  address hostname = xmppRawConnect address hostname >> xmppStartStream

xmppNewSession :: XMPPThread a -> IO (a, XMPPConState)
xmppNewSession = withNewSession . runThreaded


startTLS  :: TLS.TLSParams -> XMPPThread (Either XMPPTLSError ())
startTLS = withConnection . xmppStartTLS

auth  :: Text.Text -> Text.Text -> XMPPThread (Either String Text.Text)
auth username passwd = withConnection $ xmppSASL username passwd

connect :: HostName -> Text -> XMPPThread (Either StreamError ())
connect address hostname = withConnection $ xmppConnect address hostname
