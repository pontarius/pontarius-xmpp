{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.Xmpp.Sasl.Scram where

import           Control.Applicative((<$>))
import           Control.Monad.Error
import           Control.Monad.Trans (liftIO)
import qualified Crypto.Classes as Crypto
import qualified Crypto.HMAC as Crypto
import qualified Crypto.Hash.SHA1 as Crypto
import           Data.Binary(Binary,encode)
import           Data.Bits
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as B64
import           Data.ByteString.Char8  as BS8 (unpack)
import qualified Data.ByteString.Lazy as LBS
import           Data.List (foldl1')


import qualified Data.Binary.Builder as Build

import           Data.Maybe (maybeToList)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import           Data.Word(Word8)

import           Network.Xmpp.Sasl.Common
import           Network.Xmpp.Sasl.Types

-- | Bit-wise xor of byte strings
xorBS :: BS.ByteString -> BS.ByteString -> BS.ByteString
xorBS x y = BS.pack $ BS.zipWith xor x y

-- | Join byte strings with ","
merge :: [BS.ByteString] -> BS.ByteString
merge = BS.intercalate ","

-- | Infix concatenation of byte strings
(+++) :: BS.ByteString -> BS.ByteString -> BS.ByteString
(+++) = BS.append

-- | A nicer name for undefined, for use as a dummy token to determin
-- the hash function to use
hashToken :: (Crypto.Hash ctx hash) => hash
hashToken = undefined

-- | Salted Challenge Response Authentication Mechanism (SCRAM) SASL
-- mechanism according to RFC 5802.
--
-- This implementation is independent and polymorphic in the used hash function.
scram :: (Crypto.Hash ctx hash)
      => hash            -- ^ Dummy argument to determine the hash to use. You
                         --   can safely pass undefined or a 'hashToken' to it
      -> Text.Text       -- ^ authentication ID (username)
      -> Maybe Text.Text -- ^ authorization ID
      -> Text.Text       -- ^ password
      -> SaslM ()
scram hashToken authcid authzid' password = do
    cnonce <- liftIO $ makeNonce
    saslInit "SCRAM-SHA-1" (Just $ cFirstMessage cnonce)
    liftIO $ putStrLn "pulling challenge"
    sFirstMessage <- saslFromJust =<< pullChallenge
    liftIO $ putStrLn "pulled challenge"
    pairs <- toPairs sFirstMessage
    (nonce, salt, ic) <- fromPairs pairs cnonce
    respond . Just $ cFinalMessage nonce salt ic sFirstMessage cnonce
    liftIO $ print ic
    sFinalMessage <- pullFinalMessage
    return ()
  where
    -- We need to jump through some hoops to get a polymorphic solution
    encode :: Crypto.Hash ctx hash => hash -> hash -> BS.ByteString
    encode _hashtoken = Crypto.encode
    hash str = encode hashToken $ Crypto.hash' str
    hmac key str = encode hashToken $ Crypto.hmac' (Crypto.MacKey key) str

    authzid              = (\z -> "a=" +++ normalize z) <$> authzid'
    gs2CbindFlag         = "n" -- we don't support channel binding yet
    gs2Header            = merge $ [ gs2CbindFlag
                                    , maybe "" id authzid
                                    , ""
                                    ]
    cbindData            = "" -- we don't support channel binding yet
    cFirstMessageBare cnonce = merge [ "n=" +++ normalize authcid
                                     , "r=" +++ cnonce]
    cFirstMessage cnonce = gs2Header +++ cFirstMessageBare cnonce

    fromPairs :: Pairs
              -> BS.ByteString
              -> SaslM (BS.ByteString, BS.ByteString, Int)
    fromPairs pairs cnonce | Just nonce <- lookup "r" pairs
                           , cnonce `BS.isPrefixOf` nonce
                           , Just salt' <- lookup "s" pairs
                           , Right salt <- B64.decode salt'
                           , Just ic <- lookup "i" pairs
                           , [(i,"")] <- reads $ BS8.unpack ic
                           = return (nonce, salt, i :: Int)
    fromPairs _ _ = throwError $ AuthChallengeError

    cFinalMessage nonce salt ic sfm cnonce
        = merge [ cFinalMessageWOProof
                 , "p=" +++ B64.encode clientProof]
      where
        cFinalMessageWOProof = merge ["c=" +++ B64.encode gs2Header
                                      ,"r=" +++ nonce]
        saltedPassword       = hi (normalize password) salt ic
        clientKey            = hmac saltedPassword "Client Key"
        storedKey            = hash clientKey
        authMessage          = merge [ cFirstMessageBare cnonce
                                      , sfm
                                      , cFinalMessageWOProof
                                      ]
        clientSignature      = hmac storedKey authMessage
        clientProof          = clientKey `xorBS` clientSignature
        -- serverKey            = hmac saltedPassword "Server Key"
        -- serverSignature      = hmac serverKey authMessage

        -- helper
        hi str salt ic = foldl1' xorBS (take ic us)
          where
            u1 = hmac str (salt +++ (BS.pack [0,0,0,1]))
            us = iterate (hmac str) u1

    normalize = Text.encodeUtf8 . id -- TODO: stringprep
    base64 = B64.encode

-- | 'scram' spezialised to the SHA-1 hash function, packaged as a SaslHandler
scramSha1 :: SaslM Text.Text -> SaslHandler
scramSha1 passwd = ("SCRAM-SHA-1"
                   , \_hostname authcid authzid -> do
                       pw <- passwd
                       scram (hashToken :: Crypto.SHA1) authcid authzid pw
                   )
