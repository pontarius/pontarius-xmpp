{-# LANGUAGE NoMonomorphismRestriction, OverloadedStrings  #-}
module Network.XMPP.SASL where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.State.Strict

import qualified Crypto.Classes as CC

import qualified Data.Attoparsec.ByteString.Char8 as AP
import qualified Data.Binary as Binary
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as BL
import qualified Data.Digest.Pure.MD5 as MD5
import qualified Data.List as L
import           Data.XML.Pickle
import           Data.XML.Types

import qualified Data.Text as Text
import           Data.Text (Text)
import qualified Data.Text.Encoding as Text

import           Network.XMPP.Monad
import           Network.XMPP.Stream
import           Network.XMPP.Types

import qualified System.Random as Random


saslInitE :: Text -> Element
saslInitE mechanism =
    Element "{urn:ietf:params:xml:ns:xmpp-sasl}auth"
        [ ("mechanism", [ContentText  mechanism]) ]
        []

saslResponseE :: Text -> Element
saslResponseE resp =
    Element "{urn:ietf:params:xml:ns:xmpp-sasl}response"
    []
    [NodeContent $ ContentText resp]

saslResponse2E :: Element
saslResponse2E =
    Element "{urn:ietf:params:xml:ns:xmpp-sasl}response"
    []
    []

xmppSASL:: Text -> Text -> XMPPConMonad (Either String Text)
xmppSASL uname passwd = do
    realm <- gets sHostname
    case realm of
      Just realm' -> do
          xmppStartSASL realm' uname passwd
          modify (\s -> s{sUsername = Just uname})
          return $ Right uname
      Nothing -> return $ Left "No connection found"

xmppStartSASL  :: Text
               -> Text
               -> Text
               -> XMPPConMonad ()
xmppStartSASL realm username passwd = do
  mechanisms <- gets $ saslMechanisms . sFeatures
  unless ("DIGEST-MD5" `elem` mechanisms) .  error $ "No usable auth mechanism: " ++ show mechanisms
  pushN $ saslInitE "DIGEST-MD5"
  Right challenge <- B64.decode . Text.encodeUtf8<$> pullPickle challengePickle
  let Right pairs = toPairs challenge
  g <- liftIO $ Random.newStdGen
  pushN . saslResponseE $ createResponse g realm username passwd pairs
  challenge2 <- pullPickle (xpEither failurePickle challengePickle)
  case challenge2 of
    Left x -> error $ show x
    Right _ -> return ()
  pushN saslResponse2E
  Element "{urn:ietf:params:xml:ns:xmpp-sasl}success" [] [] <- pullE
  xmppRestartStream
  return ()

createResponse :: Random.RandomGen g
               => g
               -> Text
               -> Text
               -> Text
               -> [(BS8.ByteString, BS8.ByteString)]
               -> Text
createResponse g hostname username passwd' pairs = let
  Just qop = L.lookup "qop" pairs
  Just nonce = L.lookup "nonce" pairs
  uname = Text.encodeUtf8 username
  passwd = Text.encodeUtf8 passwd'
  realm = Text.encodeUtf8 hostname
  cnonce = BS.tail . BS.init .
           B64.encode . BS.pack . take 8 $ Random.randoms g
  nc = "00000001"
  digestURI = ("xmpp/" `BS.append` realm)
  digest = md5Digest
             uname
             realm
             passwd
             digestURI
             nc
             qop
             nonce
             cnonce
  response = BS.intercalate"," . map (BS.intercalate "=") $
   [["username"  , quote uname     ]
   ,["realm"     , quote realm     ]
   ,["nonce"     , quote nonce     ]
   ,["cnonce"    , quote cnonce    ]
   ,["nc"        ,       nc        ]
   ,["qop"       ,       qop       ]
   ,["digest-uri", quote digestURI ]
   ,["response"  ,       digest    ]
   ,["charset"   ,       "utf-8"   ]
   ]
  in Text.decodeUtf8 $ B64.encode response
  where
    quote x = BS.concat ["\"",x,"\""]

toPairs :: BS.ByteString -> Either String [(BS.ByteString, BS.ByteString)]
toPairs = AP.parseOnly . flip AP.sepBy1 (void $ AP.char ',') $ do
  AP.skipSpace
  name <- AP.takeWhile1 (/= '=')
  _ <- AP.char '='
  quote <- ((AP.char '"' >> return True) `mplus` return False)
  content <- AP.takeWhile1 (AP.notInClass ",\"" )
  when quote . void $ AP.char '"'
  return (name,content)

hash :: [BS8.ByteString] -> BS8.ByteString
hash = BS8.pack . show
       . (CC.hash' :: BS.ByteString -> MD5.MD5Digest) . BS.intercalate (":")

hashRaw :: [BS8.ByteString] -> BS8.ByteString
hashRaw = toStrict . Binary.encode
          . (CC.hash' :: BS.ByteString -> MD5.MD5Digest) . BS.intercalate (":")

toStrict :: BL.ByteString -> BS8.ByteString
toStrict = BS.concat . BL.toChunks

-- TODO: this only handles MD5-sess

md5Digest :: BS8.ByteString
          -> BS8.ByteString
          -> BS8.ByteString
          -> BS8.ByteString
          -> BS8.ByteString
          -> BS8.ByteString
          -> BS8.ByteString
          -> BS8.ByteString
          -> BS8.ByteString
md5Digest uname realm password digestURI nc qop nonce cnonce=
  let ha1 = hash [hashRaw [uname,realm,password], nonce, cnonce]
      ha2 = hash ["AUTHENTICATE", digestURI]
  in hash [ha1,nonce, nc, cnonce,qop,ha2]


-- Pickling

failurePickle :: PU [Node] (Element)
failurePickle = xpElemNodes "{urn:ietf:params:xml:ns:xmpp-sasl}failure"
                 (xpIsolate xpElemVerbatim)

challengePickle :: PU [Node] Text.Text
challengePickle =  xpElemNodes "{urn:ietf:params:xml:ns:xmpp-sasl}challenge"
                     (xpIsolate $ xpContent xpId)

