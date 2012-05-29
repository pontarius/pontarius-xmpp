{-# LANGUAGE NoMonomorphismRestriction, OverloadedStrings #-}

module Network.Xmpp.Sasl where

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

import           Network.Xmpp.Monad
import           Network.Xmpp.Stream
import           Network.Xmpp.Types
import           Network.Xmpp.Pickle

import qualified System.Random as Random

import Network.Xmpp.Sasl.Sasl
import Network.Xmpp.Sasl.DigestMD5
import Network.Xmpp.Sasl.Plain
import Network.Xmpp.Sasl.Types

-- Uses the first supported mechanism to authenticate, if any. Updates the
-- XmppConMonad state with non-password credentials and restarts the stream upon
-- success. This computation wraps an ErrorT computation, which means that
-- catchError can be used to catch any errors.
xmppSasl :: [SaslCredentials] -- ^ Acceptable authentication mechanisms and
                              --   their corresponding credentials
         -> XmppConMonad (Either AuthError ())
xmppSasl creds = runErrorT $ do
    -- Chooses the first mechanism that is acceptable by both the client and the
    -- server.
    mechanisms <- gets $ saslMechanisms . sFeatures
    let cred = L.find (\cred -> credsToName cred `elem` mechanisms) creds
    unless (isJust cred) (throwError $ AuthMechanismError mechanisms)
    case fromJust cred of
        DigestMD5Credentials authzid authcid passwd -> ErrorT $ xmppDigestMD5
            authzid
            authcid
            passwd
        PlainCredentials authzid authcid passwd -> ErrorT $ xmppPLAIN
            authzid
            authcid
            passwd
        _ -> error "xmppSasl: Mechanism not caught"
  where
    -- Converts the credentials to the appropriate mechanism name, corresponding to
    -- the XMPP mechanism attribute.
    credsToName :: SaslCredentials -> Text
    credsToName (DigestMD5Credentials _ _ _) = "DIGEST-MD5"
    credsToName (PlainCredentials _ _ _) = "PLAIN"
    credsToName c = error $ "credsToName failed for " ++ (show c)