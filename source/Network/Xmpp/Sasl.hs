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

import Network.Xmpp.Sasl.DigestMD5
import Network.Xmpp.Sasl.Plain
import Network.Xmpp.Sasl.Types

-- Uses the first supported mechanism to authenticate, if any. Updates the
-- XmppConMonad state with non-password credentials and restarts the stream upon
-- success. This computation wraps an ErrorT computation, which means that
-- catchError can be used to catch any errors.
xmppSasl :: Text.Text -- ^ Authentication identity (user name)
         -> Maybe Text.Text -- ^ Authorization identity
         -> Text.Text -- ^ Password
         -> [SaslHandler] -- ^ Acceptable authentication
                                        -- mechanisms and their corresponding
                                        -- handlers
         -> XmppConMonad (Either AuthError ())
xmppSasl authcid authzid passwd handlers = do
    -- Chooses the first mechanism that is acceptable by both the client and the
    -- server.
    mechanisms <- gets $ saslMechanisms . sFeatures
    case (filter (\(name, _) -> name `elem` mechanisms)) handlers of
        [] -> return . Left $ AuthNoAcceptableMechanism mechanisms
        (_name, handler):_ -> runSasl handler authcid authzid passwd
  where
    runSasl :: (Text.Text -> Maybe Text.Text -> Text.Text -> SaslM a)
            -> Text.Text
            -> Maybe Text.Text
            -> Text.Text
            -> XmppConMonad (Either AuthError a)
    runSasl authAction authcid authzid passwd = runErrorT $ do
        r <- authAction authcid authzid passwd
        modify (\s -> s{ sUsername = Just authcid
                       , sAuthzid = authzid
                       })
        _ <- ErrorT $ left AuthStreamError <$> xmppRestartStream
        return r