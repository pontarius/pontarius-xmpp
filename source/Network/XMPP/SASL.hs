{-# LANGUAGE NoMonomorphismRestriction, OverloadedStrings #-}

module Network.XMPP.SASL where

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

import           Network.XMPP.Monad
import           Network.XMPP.Stream
import           Network.XMPP.Types
import           Network.XMPP.Pickle

import qualified System.Random as Random

import Network.XMPP.SASL.SASL
import Network.XMPP.SASL.DIGEST_MD5
import Network.XMPP.SASL.PLAIN
import Network.XMPP.SASL.Types

-- Uses the first supported mechanism to authenticate, if any. Updates the
-- XMPPConMonad state with non-password credentials and restarts the stream upon
-- success. This computation wraps an ErrorT computation, which means that
-- catchError can be used to catch any errors.
xmppSASL :: [SASLCredentials] -- ^ Acceptable authentication mechanisms and
                              --   their corresponding credentials
         -> XMPPConMonad (Either AuthError ())
xmppSASL creds = runErrorT $ do
    -- Chooses the first mechanism that is acceptable by both the client and the
    -- server.
    mechanisms <- gets $ saslMechanisms . sFeatures
    let cred = L.find (\cred -> credsToName cred `elem` mechanisms) creds
    unless (isJust cred) (throwError $ AuthMechanismError mechanisms)
    case fromJust cred of
        DIGEST_MD5Credentials authzid authcid passwd -> ErrorT $ xmppDIGEST_MD5
            authzid
            authcid
            passwd
        PLAINCredentials authzid authcid passwd -> ErrorT $ xmppPLAIN
            authzid
            authcid
            passwd
        _ -> error "xmppSASL: Mechanism not caught"
  where
    -- Converts the credentials to the appropriate mechanism name, corresponding to
    -- the XMPP mechanism attribute.
    credsToName :: SASLCredentials -> Text
    credsToName (DIGEST_MD5Credentials _ _ _) = "DIGEST-MD5"
    credsToName (PLAINCredentials _ _ _) = "PLAIN"
    credsToName c = error $ "credsToName failed for " ++ (show c)