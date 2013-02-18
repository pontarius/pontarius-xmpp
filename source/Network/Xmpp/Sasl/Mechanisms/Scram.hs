{-# OPTIONS_HADDOCK hide #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.Xmpp.Sasl.Mechanisms.Scram
  where

import           Control.Applicative ((<$>))
import           Control.Monad.Error
import           Control.Monad.Trans (liftIO)
import qualified Crypto.Classes as Crypto
import qualified Crypto.HMAC as Crypto
import qualified Crypto.Hash.SHA1 as Crypto
import           Data.Binary(Binary,encode)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as B64
import           Data.ByteString.Char8  as BS8 (unpack)
import qualified Data.ByteString.Lazy as LBS
import           Data.List (foldl1', genericTake)

import qualified Data.Binary.Builder as Build

import           Data.Maybe (maybeToList)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import           Data.Word(Word8)

import           Network.Xmpp.Sasl.Common
import           Network.Xmpp.Sasl.StringPrep
import           Network.Xmpp.Sasl.Types
import           Network.Xmpp.Types

import           Control.Monad.State.Strict
import           Control.Concurrent.STM

-- | A nicer name for undefined, for use as a dummy token to determin
-- the hash function to use
hashToken :: (Crypto.Hash ctx hash) => hash
hashToken = undefined

-- | Salted Challenge Response Authentication Mechanism (SCRAM) SASL
-- mechanism according to RFC 5802.
--
-- This implementation is independent and polymorphic in the used hash function.
scram :: (Crypto.Hash ctx hash)
      => hash            -- ^ Dummy argument to determine the hash to use; you
                         --   can safely pass undefined or a 'hashToken' to it
      -> Text.Text       -- ^ Authentication ID (user name)
      -> Maybe Text.Text -- ^ Authorization ID
      -> Text.Text       -- ^ Password
      -> ErrorT AuthFailure (StateT Stream IO) ()
scram hashToken authcid authzid password = do
    (ac, az, pw) <- prepCredentials authcid authzid password
    scramhelper hashToken ac az pw
  where
    scramhelper hashToken authcid authzid' password = do
        cnonce <- liftIO $ makeNonce
        saslInit "SCRAM-SHA-1" (Just $ cFirstMessage cnonce)
        sFirstMessage <- saslFromJust =<< pullChallenge
        pairs <- toPairs sFirstMessage
        (nonce, salt, ic) <- fromPairs pairs cnonce
        let (cfm, v) = cFinalMessageAndVerifier nonce salt ic sFirstMessage cnonce
        respond $ Just cfm
        finalPairs <- toPairs =<< saslFromJust =<< pullFinalMessage
        unless (lookup "v" finalPairs == Just v) $ throwError AuthServerAuthFailure
        return ()
      where
        -- We need to jump through some hoops to get a polymorphic solution
        encode :: Crypto.Hash ctx hash => hash -> hash -> BS.ByteString
        encode _hashtoken = Crypto.encode

        hash :: BS.ByteString -> BS.ByteString
        hash str = encode hashToken $ Crypto.hash' str

        hmac :: BS.ByteString -> BS.ByteString -> BS.ByteString
        hmac key str = encode hashToken $ Crypto.hmac' (Crypto.MacKey key) str

        authzid :: Maybe BS.ByteString
        authzid              = (\z -> "a=" +++ Text.encodeUtf8 z) <$> authzid'

        gs2CbindFlag :: BS.ByteString
        gs2CbindFlag         = "n" -- we don't support channel binding yet

        gs2Header :: BS.ByteString
        gs2Header            = merge $ [ gs2CbindFlag
                                        , maybe "" id authzid
                                        , ""
                                        ]
        cbindData :: BS.ByteString
        cbindData            = "" -- we don't support channel binding yet

        cFirstMessageBare :: BS.ByteString -> BS.ByteString
        cFirstMessageBare cnonce = merge [ "n=" +++ Text.encodeUtf8 authcid
                                         , "r=" +++ cnonce]
        cFirstMessage :: BS.ByteString -> BS.ByteString
        cFirstMessage cnonce = gs2Header +++ cFirstMessageBare cnonce

        fromPairs :: Pairs
                  -> BS.ByteString
                  -> ErrorT AuthFailure (StateT Stream IO) (BS.ByteString, BS.ByteString, Integer)
        fromPairs pairs cnonce | Just nonce <- lookup "r" pairs
                               , cnonce `BS.isPrefixOf` nonce
                               , Just salt' <- lookup "s" pairs
                               , Right salt <- B64.decode salt'
                               , Just ic <- lookup "i" pairs
                               , [(i,"")] <- reads $ BS8.unpack ic
                               = return (nonce, salt, i)
        fromPairs _ _ = throwError $ AuthChallengeFailure

        cFinalMessageAndVerifier :: BS.ByteString
                                 -> BS.ByteString
                                 -> Integer
                                 -> BS.ByteString
                                 -> BS.ByteString
                                 -> (BS.ByteString, BS.ByteString)
        cFinalMessageAndVerifier nonce salt ic sfm cnonce
            =  (merge [ cFinalMessageWOProof
                      , "p=" +++ B64.encode clientProof
                      ]
              , B64.encode serverSignature
              )
          where
            cFinalMessageWOProof :: BS.ByteString
            cFinalMessageWOProof = merge [ "c=" +++ B64.encode gs2Header
                                         , "r=" +++ nonce]

            saltedPassword :: BS.ByteString
            saltedPassword       = hi (Text.encodeUtf8 password) salt ic

            clientKey :: BS.ByteString
            clientKey            = hmac saltedPassword "Client Key"

            storedKey :: BS.ByteString
            storedKey            = hash clientKey

            authMessage :: BS.ByteString
            authMessage          = merge [ cFirstMessageBare cnonce
                                         , sfm
                                         , cFinalMessageWOProof
                                         ]

            clientSignature :: BS.ByteString
            clientSignature      = hmac storedKey authMessage

            clientProof :: BS.ByteString
            clientProof          = clientKey `xorBS` clientSignature

            serverKey :: BS.ByteString
            serverKey            = hmac saltedPassword "Server Key"

            serverSignature :: BS.ByteString
            serverSignature      = hmac serverKey authMessage

            -- helper
            hi :: BS.ByteString -> BS.ByteString -> Integer -> BS.ByteString
            hi str salt ic = foldl1' xorBS (genericTake ic us)
              where
                u1 = hmac str (salt +++ (BS.pack [0,0,0,1]))
                us = iterate (hmac str) u1

scramSha1 :: Text.Text  -- ^ username
          -> Maybe Text.Text -- ^ authorization ID
          -> Text.Text   -- ^ password
          -> SaslHandler
scramSha1 authcid authzid passwd =
    ( "SCRAM-SHA-1"
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
            (scram (hashToken :: Crypto.SHA1) authcid authzid passwd)
        case r of
          Left (AuthStreamFailure e) -> return $ Left e
          Left e -> return $ Right $ Just e
          Right () -> return $ Right $ Nothing
    )
