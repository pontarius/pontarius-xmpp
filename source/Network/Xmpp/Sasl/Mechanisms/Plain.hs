{-# OPTIONS_HADDOCK hide #-}
-- Implementation of the PLAIN Simple Authentication and Security Layer (SASL)
-- Mechanism, http://tools.ietf.org/html/rfc4616.

{-# LANGUAGE OverloadedStrings #-}

module Network.Xmpp.Sasl.Mechanisms.Plain
    ( plain
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

import           Data.XML.Pickle

import qualified Data.ByteString as BS

import           Data.XML.Types

import           Network.Xmpp.Stream
import           Network.Xmpp.Types

import qualified System.Random as Random

import           Data.Maybe (fromMaybe)
import qualified Data.Text as Text

import           Network.Xmpp.Sasl.Common
import           Network.Xmpp.Sasl.Types

import           Control.Concurrent.STM

-- TODO: stringprep
xmppPlain :: Text.Text -- ^ Password
          -> Maybe Text.Text -- ^ Authorization identity (authzid)
          -> Text.Text -- ^ Authentication identity (authcid)
          -> ErrorT AuthFailure (StateT Stream IO) ()
xmppPlain authcid authzid password  = do
    (ac, az, pw) <- prepCredentials authcid authzid password
    _ <- saslInit "PLAIN" ( Just $ plainMessage ac az pw)
    _ <- pullSuccess
    return ()
  where
    -- Converts an optional authorization identity, an authentication identity,
    -- and a password to a \NUL-separated PLAIN message.
    plainMessage :: Text.Text -- Authorization identity (authzid)
                 -> Maybe Text.Text -- Authentication identity (authcid)
                 -> Text.Text -- Password
                 -> BS.ByteString -- The PLAIN message
    plainMessage authcid authzid passwd = BS.concat $
                                            [ authzid'
                                            , "\NUL"
                                            , Text.encodeUtf8 $ authcid
                                            , "\NUL"
                                            , Text.encodeUtf8 $ passwd
                                            ]
      where
        authzid' = maybe "" Text.encodeUtf8 authzid

plain :: Text.Text -- ^ authentication ID (username)
      -> Maybe Text.Text -- ^ authorization ID
      -> Text.Text -- ^ password
      -> SaslHandler
plain authcid authzid passwd =
    ( "PLAIN"
    , \stream -> do
        stream_ <- atomically $ readTMVar stream
        r <- runErrorT $ do
          -- Alrighty! The problem here is that `scramSha1' runs in the
          -- `IO (Either XmppFailure (Maybe AuthFailure))' monad, while we need
          -- to call an `ErrorT AuthFailure (StateT Stream IO) ()' calculation.
          -- The key is to use `mapErrorT', which is called with the following
          -- ypes:
          -- 
          -- mapErrorT :: (StateT Stream IO (Either AuthError ()) -> IO (Either AuthError ()))
          --           -> ErrorT AuthError (StateT Stream IO) ()
          --           -> ErrorT AuthError IO ()
          mapErrorT
            (\s -> runStateT s stream_ >>= \(r, _) -> return r)
            (xmppPlain authcid authzid passwd)
        case r of
          Left (AuthStreamFailure e) -> return $ Left e
          Left e -> return $ Right $ Just e
          Right () -> return $ Right $ Nothing
    )


