{-# LANGUAGE OverloadedStrings #-}

module Network.XMPP.SASL.DIGEST_MD5 where

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

import           Network.XMPP.Monad
import           Network.XMPP.Stream
import           Network.XMPP.Types
import           Network.XMPP.Pickle

import qualified System.Random as Random

import Network.XMPP.SASL.SASL
import Network.XMPP.SASL.Types

xmppDIGEST_MD5 :: Maybe Text -- Authorization identity (authzid)
               -> Text -- Authentication identity (authzid)
               -> Text -- Password (authzid)
               -> XMPPConMonad (Either AuthError ())
xmppDIGEST_MD5 authzid authcid passwd = runErrorT $ do
    realm <- gets sHostname
    case realm of
        Just realm' -> do
            ErrorT $ xmppDIGEST_MD5' realm'
            -- TODO: Save authzid
            modify (\s -> s{sUsername = Just authcid})
        Nothing -> throwError AuthConnectionError
  where
    xmppDIGEST_MD5' :: Text -- ^ SASL realm
                    -> XMPPConMonad (Either AuthError ())
    xmppDIGEST_MD5' realm = runErrorT $ do
        -- Push element and receive the challenge (in XMPPConMonad).
        _ <- lift . pushElement $ saslInitE "DIGEST-MD5" Nothing -- TODO: Check boolean?
        challenge' <- lift $ B64.decode . Text.encodeUtf8 <$>
            pullPickle challengePickle
        challenge <- case challenge' of
            Left _e -> throwError AuthChallengeError
            Right r -> return r
        pairs <- case toPairs challenge of
            Left _ -> throwError AuthChallengeError
            Right p -> return p
        g <- liftIO Random.newStdGen
        _ <- lift . pushElement . -- TODO: Check boolean?
            saslResponseE $ createResponse g realm pairs
        challenge2 <- lift $ pullPickle (xpEither failurePickle challengePickle)
        case challenge2 of
            Left _x -> throwError AuthXmlError
            Right _ -> return ()
        lift $ pushElement saslResponse2E
        e <- lift pullElement
        case e of
            Element "{urn:ietf:params:xml:ns:xmpp-sasl}success" [] [] ->
                return ()
            _ -> throwError AuthXmlError -- TODO: investigate
        -- The SASL authentication has succeeded; the stream is restarted.
        _ <- ErrorT $ left AuthStreamError <$> xmppRestartStream
        return ()
    -- Produce the response to the challenge.
    createResponse :: Random.RandomGen g
                   => g
                   -> Text
                   -> [(BS8.ByteString, BS8.ByteString)] -- Pairs
                   -> Text
    createResponse g hostname pairs = let
        Just qop   = L.lookup "qop" pairs
        Just nonce = L.lookup "nonce" pairs
        uname_     = Text.encodeUtf8 authcid
        passwd_    = Text.encodeUtf8 passwd
        -- Using Int instead of Word8 for random 1.0.0.0 (GHC 7) compatibility.
        cnonce     = BS.tail . BS.init .
                         B64.encode . BS.pack . map toWord8 .
                         take 8 $ Random.randoms g
        nc         = "00000001"
        digestURI  = "xmpp/" `BS.append` Text.encodeUtf8 hostname
        digest     = md5Digest
            uname_
            (lookup "realm" pairs)
            passwd_
            digestURI
            nc
            qop
            nonce
            cnonce
        response = BS.intercalate "," . map (BS.intercalate "=") $
            [["username", quote uname_]] ++
                case L.lookup "realm" pairs of
                    Just realm -> [["realm" , quote realm ]]
                    Nothing -> [] ++
                        [ ["nonce"     , quote nonce    ]
                        , ["cnonce"    , quote cnonce   ]
                        , ["nc"        ,       nc       ]
                        , ["qop"       ,       qop      ]
                        , ["digest-uri", quote digestURI]
                        , ["response"  ,       digest   ]
                        , ["charset"   ,       "utf-8"  ]
                        ]
        in Text.decodeUtf8 $ B64.encode response
    quote :: BS8.ByteString -> BS8.ByteString
    quote x = BS.concat ["\"",x,"\""]
    toWord8 :: Int -> Word8
    toWord8 x = fromIntegral x :: Word8
    hash :: [BS8.ByteString] -> BS8.ByteString
    hash = BS8.pack . show
           . (CC.hash' :: BS.ByteString -> MD5.MD5Digest) . BS.intercalate (":")
    hashRaw :: [BS8.ByteString] -> BS8.ByteString
    hashRaw = toStrict . Binary.encode .
        (CC.hash' :: BS.ByteString -> MD5.MD5Digest) . BS.intercalate (":")
    toStrict :: BL.ByteString -> BS8.ByteString
    toStrict = BS.concat . BL.toChunks
    -- TODO: this only handles MD5-sess
    md5Digest :: BS8.ByteString
              -> Maybe BS8.ByteString
              -> BS8.ByteString
              -> BS8.ByteString
              -> BS8.ByteString
              -> BS8.ByteString
              -> BS8.ByteString
              -> BS8.ByteString
              -> BS8.ByteString
    md5Digest uname realm password digestURI nc qop nonce cnonce =
      let ha1 = hash [ hashRaw [uname, maybe "" id realm, password]
                     , nonce
                     , cnonce
                     ]
          ha2 = hash ["AUTHENTICATE", digestURI]
      in hash [ha1, nonce, nc, cnonce, qop, ha2]