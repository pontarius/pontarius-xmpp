{-# OPTIONS_HADDOCK hide #-}
-- Implementation of the PLAIN Simple Authentication and Security Layer (SASL)
-- Mechanism, http://tools.ietf.org/html/rfc4616.

{-# LANGUAGE OverloadedStrings #-}

module Network.Xmpp.Sasl.Mechanisms.Plain
    ( plain
    ) where

import           Control.Monad.Except
import           Control.Monad.State.Strict
import qualified Data.ByteString as BS
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import           Network.Xmpp.Sasl.Common
import           Network.Xmpp.Sasl.Types
import           Network.Xmpp.Types

-- TODO: stringprep
xmppPlain :: Text.Text -- ^ Password
          -> Maybe Text.Text -- ^ Authorization identity (authzid)
          -> Text.Text -- ^ Authentication identity (authcid)
          -> ExceptT AuthFailure (StateT StreamState IO) ()
xmppPlain authcid' authzid' password  = do
    (ac, az, pw) <- prepCredentials authcid' authzid' password
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
    plainMessage authcid _authzid passwd = BS.concat $
                                             [ authzid''
                                             , "\NUL"
                                             , Text.encodeUtf8 $ authcid
                                             , "\NUL"
                                             , Text.encodeUtf8 $ passwd
                                             ]
      where
        authzid'' = maybe "" Text.encodeUtf8 authzid'

plain :: Username -- ^ authentication ID (username)
      -> Maybe AuthZID -- ^ authorization ID
      -> Password -- ^ password
      -> SaslHandler
plain authcid authzid passwd =
    ( "PLAIN"
    , do
          r <- runExceptT $ xmppPlain authcid authzid passwd
          case r of
              Left (AuthStreamFailure e) -> return $ Left e
              Left e -> return $ Right $ Just e
              Right () -> return $ Right Nothing
    )
