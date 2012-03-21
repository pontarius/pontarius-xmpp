{-# LANGUAGE NoMonomorphismRestriction, OverloadedStrings  #-}
module Network.XMPP.SASL where

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.State

import qualified Crypto.Classes as CC

import qualified Data.Attoparsec.ByteString.Char8 as AP
import qualified Data.Binary as Binary
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BL8
import qualified Data.ByteString.Base64 as B64
import qualified Data.List as L
import qualified Data.Digest.Pure.MD5 as MD5
import Data.List

import qualified Data.Text as Text
import Data.Text (Text)
import qualified Data.Text.Encoding as Text

import Network.XMPP.Monad
import Network.XMPP.Pickle
import Network.XMPP.Stream
import Network.XMPP.Types

import Numeric --

import qualified System.Random as Random

import Text.XML.Expat.Pickle
import Text.XML.Expat.Tree

saslInitE :: Text -> Node Text Text
saslInitE mechanism =
    Element "auth"
        [ ("xmlns","urn:ietf:params:xml:ns:xmpp-sasl")
        ,  ("mechanism", mechanism)
        ]
        []

saslResponseE :: Text -> Node Text Text
saslResponseE resp =
    Element "response"
    [("xmlns","urn:ietf:params:xml:ns:xmpp-sasl")]
    [Text resp]

saslResponse2E :: Node Text Text
saslResponse2E =
    Element "response"
    [("xmlns","urn:ietf:params:xml:ns:xmpp-sasl")]
    []

xmppSASL  :: Text -> XMPPMonad ()
xmppSASL passwd = do
  mechanisms <- gets $ saslMechanisms . sFeatures
  unless ("DIGEST-MD5" `elem` mechanisms) $ error "No usable auth mechanism"
  pushN $ saslInitE "DIGEST-MD5"
  Right challenge <- B64.decode . Text.encodeUtf8<$> pullPickle challengePickle
  let Right pairs = toPairs challenge
  pushN . saslResponseE =<< createResponse passwd pairs
  challenge2 <- pullPickle (xpEither failurePickle challengePickle)
  case challenge2 of
    Left x -> error $ show x
    Right c -> return ()
  pushN saslResponse2E
  Element "success" [("xmlns","urn:ietf:params:xml:ns:xmpp-sasl")] [] <- pullE
  xmppRestartStream
  return ()

createResponse :: Text -> [(BS8.ByteString, BS8.ByteString)] -> XMPPMonad Text
createResponse passwd' pairs = do
  let Just qop = L.lookup "qop" pairs
  let Just nonce = L.lookup "nonce" pairs
  uname <- Text.encodeUtf8 <$> gets sUsername
  let passwd = Text.encodeUtf8 passwd'
  realm <- Text.encodeUtf8 <$> gets sHostname
  g <- liftIO $ Random.newStdGen
  let cnonce = BS.tail . BS.init .
               B64.encode . BS.pack . take 8 $ Random.randoms g
  let nc = "00000001"
  let digestURI = ("xmpp/" `BS.append` realm)
  let digest = md5Digest
                 uname
                 realm
                 passwd
                 digestURI
                 nc
                 qop
                 nonce
                 cnonce
  let response = BS.intercalate"," . map (BS.intercalate "=") $
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
  return . Text.decodeUtf8 $ B64.encode response
  where quote x = BS.concat ["\"",x,"\""]

toPairs :: BS.ByteString -> Either String [(BS.ByteString, BS.ByteString)]
toPairs = AP.parseOnly . flip AP.sepBy1 (void $ AP.char ',') $ do
  AP.skipSpace
  name <- AP.takeWhile1 (/= '=')
  AP.char '='
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

toStrict = BS.concat . BL.toChunks
-- TODO: this only handles MD5-sess
md5Digest uname realm password digestURI nc qop nonce cnonce=
  let ha1 = hash [hashRaw [uname,realm,password], nonce, cnonce]
      ha2 = hash ["AUTHENTICATE", digestURI]
  in hash [ha1,nonce, nc, cnonce,qop,ha2]


-- Pickling

failurePickle :: PU [Node Text Text] (Node Text Text)
failurePickle = ignoreAttrs $
  xpElem "failure"
     (xpAttrFixed "xmlns" "urn:ietf:params:xml:ns:xmpp-sasl")
     (xpTree)

challengePickle :: PU [Node Text.Text Text.Text] Text.Text
challengePickle = ignoreAttrs $
  xpElem "challenge"
    (xpAttrFixed "xmlns" "urn:ietf:params:xml:ns:xmpp-sasl")
    (xpContent xpText0)

