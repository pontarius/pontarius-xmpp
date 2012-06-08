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

xorBS x y = BS.pack $ BS.zipWith xor x y
merge = BS.intercalate ","

type Hash = BS.ByteString -> BS.ByteString
type Hmac = BS.ByteString -> BS.ByteString -> BS.ByteString


-- -- mKey :: Crypto.Hash ctx d => d -> BS.ByteString -> MacKey ctx d
-- -- mKey x k = Crypto.MacKey k

(+++) :: BS.ByteString -> BS.ByteString -> BS.ByteString
(+++) = BS.append

hashToken :: (Crypto.Hash ctx hash) => hash
hashToken = undefined

scram :: (Crypto.Hash ctx hash)
      => hash -- ^ Dummy argument to determine the hash to use. You
              -- can safely pass undefined or a 'hashToken' to it
      -> Text.Text
      -> Maybe Text.Text
      -> Text.Text
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

--        toOectets l = encode $ x


     -- SaltedPassword  := Hi(Normalize(password), salt, i)
     -- ClientKey       := HMAC(SaltedPassword, "Client Key")
     -- StoredKey       := H(ClientKey)
     -- AuthMessage     := client-first-message-bare + "," +
     --                    server-first-message + "," +
     --                    client-final-message-without-proof
     -- ClientSignature := HMAC(StoredKey, AuthMessage)
     -- ClientProof     := ClientKey XOR ClientSignature
     -- ServerKey       := HMAC(SaltedPassword, "Server Key")
     -- ServerSignature := HMAC(ServerKey, AuthMessage)


    normalize = Text.encodeUtf8 . id -- TODO: stringprep
    base64 = B64.encode

scramSha1 :: SaslM Text.Text -> SaslHandler
scramSha1 passwd = ("SCRAM-SHA-1"
                   , \_hostname authcid authzid -> do
                       pw <- passwd
                       scram (hashToken :: Crypto.SHA1) authcid authzid pw
                   )

showBits x = [if testBit x i then '1' else '0' | i <- [0.. bitSize x -1]]

toOctets :: (Binary a) => a -> [Word8]
toOctets x = LBS.unpack . encode $ x

intToFourWord8s i = let w8s =  toOctets $ i
                    in drop (length w8s -4) w8s