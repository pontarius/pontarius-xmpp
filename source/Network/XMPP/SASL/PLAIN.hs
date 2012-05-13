-- Implementation of the PLAIN Simple Authentication and Security Layer (SASL)
-- Mechanism, http://tools.ietf.org/html/rfc4616.

{-# LANGUAGE OverloadedStrings #-}

module Network.XMPP.SASL.PLAIN where

import           Control.Applicative
import           Control.Arrow (left)
import           Control.Monad
import           Control.Monad.Error
import           Control.Monad.State.Strict
import           Data.Maybe (fromJust, isJust)

import qualified Crypto.Classes as CC

import qualified Data.Binary as Binary
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as BL
import qualified Data.Digest.Pure.MD5 as MD5
import qualified Data.List as L
import           Data.Word (Word8)

import qualified Data.Text as Text
import           Data.Text (Text)
import qualified Data.Text.Encoding as Text

import Data.XML.Pickle

import qualified Data.ByteString as BS

import Data.XML.Types

import           Network.XMPP.Monad
import           Network.XMPP.Stream
import           Network.XMPP.Types
import           Network.XMPP.Pickle

import qualified System.Random as Random

import Data.Maybe (fromMaybe)
import qualified Data.Text as T

import Network.XMPP.SASL.SASL
import Network.XMPP.SASL.Types

xmppPLAIN :: Maybe T.Text
          -> T.Text
          -> T.Text
          -> XMPPConMonad (Either AuthError ())
xmppPLAIN authzid authcid passwd = runErrorT $ do
    _ <- lift . pushElement $ saslInitE "PLAIN" $ -- TODO: Check boolean?
        Just $ plainMessage authzid authcid passwd
    lift $ pushElement saslResponse2E
    e <- lift pullElement
    case e of
        Element "{urn:ietf:params:xml:ns:xmpp-sasl}success" [] [] ->
            return ()
        _ -> throwError AuthXmlError -- TODO: investigate
    -- The SASL authentication has succeeded; the stream is restarted.
    _ <- ErrorT $ left AuthStreamError <$> xmppRestartStream
    return ()
  where
    -- Converts an optional authorization identity, an authentication identity,
    -- and a password to a \NUL-separated PLAIN message.
    plainMessage :: Maybe T.Text -- Authorization identity (authzid)
                 -> T.Text -- Authentication identity (authcid)
                 -> T.Text -- Password
                 -> T.Text -- The PLAIN message
    plainMessage authzid authcid passwd =
        let authzid' = fromMaybe "" authzid in
            T.concat [authzid', "\NUL", authcid, "\NUL", passwd]