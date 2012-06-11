-- Implementation of the PLAIN Simple Authentication and Security Layer (SASL)
-- Mechanism, http://tools.ietf.org/html/rfc4616.

{-# LANGUAGE OverloadedStrings #-}

module Network.Xmpp.Sasl.Plain where

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

import           Network.Xmpp.Monad
import           Network.Xmpp.Stream
import           Network.Xmpp.Types
import           Network.Xmpp.Pickle

import qualified System.Random as Random

import Data.Maybe (fromMaybe)
import qualified Data.Text as Text

import Network.Xmpp.Sasl.Common
import Network.Xmpp.Sasl.Types

-- TODO: stringprep
xmppPlain :: Text.Text
          -> Maybe Text.Text
          -> Text.Text
          -> SaslM ()
xmppPlain authcid authzid passwd  = do
    _ <- saslInit "PLAIN" ( Just $ plainMessage authzid authcid passwd)
    _ <- pullSuccess
    return ()
  where
    -- Converts an optional authorization identity, an authentication identity,
    -- and a password to a \NUL-separated PLAIN message.
    plainMessage :: Maybe Text.Text -- Authorization identity (authzid)
                 -> Text.Text -- Authentication identity (authcid)
                 -> Text.Text -- Password
                 -> BS.ByteString -- The PLAIN message
    plainMessage authzid authcid passwd = BS.concat $
                                            [ authzid'
                                            , "\NUL"
                                            , Text.encodeUtf8 $ authcid
                                            , "\NUL"
                                            , Text.encodeUtf8 $ passwd
                                            ]
      where
        authzid' = maybe "" Text.encodeUtf8 authzid

plain :: Text.Text -> Maybe Text.Text -> Text.Text -> SaslHandler
plain authcid authzid passwd = ("PLAIN", xmppPlain authcid authzid passwd)