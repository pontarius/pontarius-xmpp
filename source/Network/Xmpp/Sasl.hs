{-# OPTIONS_HADDOCK hide #-}
{-# LANGUAGE NoMonomorphismRestriction, OverloadedStrings #-}

module Network.Xmpp.Sasl
    ( xmppSasl
    , digestMd5
    , scramSha1
    , plain
    ) where

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

import           Network.Xmpp.Connection
import           Network.Xmpp.Pickle
import           Network.Xmpp.Stream
import           Network.Xmpp.Types

import qualified System.Random as Random

import           Network.Xmpp.Sasl.Types
import           Network.Xmpp.Sasl.Mechanisms

-- | Uses the first supported mechanism to authenticate, if any. Updates the
-- state with non-password credentials and restarts the stream upon
-- success.
xmppSasl :: [SaslHandler] -- ^ Acceptable authentication mechanisms and their
                       -- corresponding handlers
         -> XmppConMonad (Either AuthError ())
xmppSasl handlers = do
    -- Chooses the first mechanism that is acceptable by both the client and the
    -- server.
    mechanisms <- gets $ saslMechanisms . sFeatures
    case (filter (\(name, _) -> name `elem` mechanisms)) handlers of
        [] -> return . Left $ AuthNoAcceptableMechanism mechanisms
        (_name, handler):_ -> runErrorT $ do
            cs <- gets sConnectionState
            case cs of
                XmppConnectionClosed -> throwError AuthConnectionError
                _ -> do
                    r <- handler
                    _ <- ErrorT $ left AuthStreamError <$> xmppRestartStream
                    return r
