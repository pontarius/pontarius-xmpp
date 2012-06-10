{-# LANGUAGE OverloadedStrings #-}

module Network.Xmpp.Sasl.DigestMD5 where

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

import qualified Data.Text as Text
import           Data.Text (Text)
import qualified Data.Text.Encoding as Text

import Data.XML.Pickle

import qualified Data.ByteString as BS

import Data.XML.Types

import           Network.Xmpp.Monad
import           Network.Xmpp.Stream
import           Network.Xmpp.Types
import           Network.Xmpp.Pickle


import Network.Xmpp.Sasl.Common
import Network.Xmpp.Sasl.Types



xmppDigestMD5 :: Maybe Text -- Authorization identity (authzid)
               -> Text -- Authentication identity (authzid)
               -> Text -- Password (authzid)
               -> XmppConMonad (Either AuthError ())
xmppDigestMD5 authzid authcid passwd = runErrorT $ do
    hn <- gets sHostname
    case hn of
        Just hn' -> do
            xmppDigestMD5' hn'
            -- TODO: Save authzid
            modify (\s -> s{sUsername = Just authcid})
        Nothing -> throwError AuthConnectionError
  where
    xmppDigestMD5' :: Text -> SaslM ()
    xmppDigestMD5' hostname = do
        -- Push element and receive the challenge (in SaslM).
        _ <- saslInit "DIGEST-MD5" Nothing -- TODO: Check boolean?
        pairs <- toPairs =<< saslFromJust =<< pullChallenge
        cnonce <- liftIO $ makeNonce
        _b <- respond . Just $ createResponse hostname pairs cnonce
        challenge2 <- pullFinalMessage
        _ <- ErrorT $ left AuthStreamError <$> xmppRestartStream
        return ()
    -- Produce the response to the challenge.
    createResponse :: Text
                   -> Pairs
                   -> BS.ByteString -- nonce
                   -> BS.ByteString
    createResponse hostname pairs cnonce = let
        Just qop   = L.lookup "qop" pairs
        Just nonce = L.lookup "nonce" pairs
        uname_     = Text.encodeUtf8 authcid
        passwd_    = Text.encodeUtf8 passwd
        -- Using Int instead of Word8 for random 1.0.0.0 (GHC 7) compatibility.

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
        in B64.encode response
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