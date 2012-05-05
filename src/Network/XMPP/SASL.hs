{-# LANGUAGE NoMonomorphismRestriction, OverloadedStrings  #-}

module Network.XMPP.SASL where

import           Control.Applicative
import           Control.Arrow (left)
import           Control.Monad
import           Control.Monad.Error
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
import           Data.Word (Word8)
import           Data.XML.Pickle
import           Data.XML.Types

import qualified Data.Text as Text
import           Data.Text (Text)
import qualified Data.Text.Encoding as Text

import           Network.XMPP.Monad
import           Network.XMPP.Stream
import           Network.XMPP.Types
import           Network.XMPP.Pickle

import qualified System.Random as Random

data AuthError = AuthXmlError
               | AuthMechanismError [Text] -- ^ Wraps mechanisms offered
               | AuthChallengeError
               | AuthStreamError StreamError -- ^ Stream error on stream restart
               | AuthConnectionError -- ^ No host name set in state
               | AuthError -- General instance used for the Error instance
                 deriving Show

instance Error AuthError where
    noMsg = AuthError

-- Uses the DIGEST-MD5 method (if available) to authenticate. Updates the
-- sUsername XMPPConMonad field with a `Just' value and restarts the stream upon
-- success. This computation wraps an ErrorT computation, which means that
-- catchError can be used to catch any errors.
xmppSASL :: Text -- ^ User name
         -> Text -- ^ Password
         -> XMPPConMonad (Either AuthError ())
xmppSASL uname passwd = runErrorT $ do
    realm <- gets sHostname
    case realm of
        Just realm' -> do
            ErrorT $ xmppStartSASL realm'
            modify (\s -> s{sUsername = Just uname})
        Nothing -> throwError AuthConnectionError
  where
    xmppStartSASL :: Text -- ^ SASL realm
                  -> XMPPConMonad (Either AuthError ())
    xmppStartSASL realm = runErrorT $ do
        mechanisms <- gets $ saslMechanisms . sFeatures
        unless ("DIGEST-MD5" `elem` mechanisms) .
            throwError $ AuthMechanismError mechanisms
        -- Push element and receive the challenge (in XMPPConMonad).
        _ <- lift . pushN $ saslInitE "DIGEST-MD5" -- TODO: Check boolean?
        challenge' <- lift $ B64.decode . Text.encodeUtf8 <$>
            pullPickle challengePickle
        challenge <- case challenge' of
            Left _e -> throwError AuthChallengeError
            Right r -> return r
        pairs <- case toPairs challenge of
            Left _ -> throwError AuthChallengeError
            Right p -> return p
        g <- liftIO Random.newStdGen
        _ <- lift . pushN . -- TODO: Check boolean?
            saslResponseE $ createResponse g realm pairs
        challenge2 <- lift $ pullPickle (xpEither failurePickle challengePickle)
        case challenge2 of
            Left _x -> throwError AuthXmlError
            Right _ -> return ()
        lift $ pushN saslResponse2E
        e <- lift pullElement
        case e of
            Element "{urn:ietf:params:xml:ns:xmpp-sasl}success" [] [] ->
                return ()
            _ -> throwError AuthXmlError -- TODO: investigate
        -- The SASL authentication has succeeded; the stream is restarted.
        _ <- ErrorT $ left AuthStreamError <$> xmppRestartStream
        return ()
    -- The <auth xmlns='urn:ietf:params:xml:ns:xmpp-sasl'/> element.
    saslInitE :: Text -> Element
    saslInitE mechanism =
        Element "{urn:ietf:params:xml:ns:xmpp-sasl}auth"
            [("mechanism", [ContentText mechanism])]
            []
    -- SASL response with text payload.
    saslResponseE :: Text -> Element
    saslResponseE resp =
        Element "{urn:ietf:params:xml:ns:xmpp-sasl}response"
        []
        [NodeContent $ ContentText resp]
    -- SASL response without payload.
    saslResponse2E :: Element
    saslResponse2E =
        Element "{urn:ietf:params:xml:ns:xmpp-sasl}response"
        []
        []
    -- Parses the incoming SASL data to a mapped list of pairs.
    toPairs :: BS.ByteString -> Either String [(BS.ByteString, BS.ByteString)]
    toPairs = AP.parseOnly . flip AP.sepBy1 (void $ AP.char ',') $ do
        AP.skipSpace
        name <- AP.takeWhile1 (/= '=')
        _ <- AP.char '='
        quote <- ((AP.char '"' >> return True) `mplus` return False)
        content <- AP.takeWhile1 (AP.notInClass [',', '"'])
        when quote . void $ AP.char '"'
        return (name, content)
    -- Produce the response to the challenge.
    createResponse :: Random.RandomGen g
                   => g
                   -> Text
                   -> [(BS8.ByteString, BS8.ByteString)] -- Pairs
                   -> Text
    createResponse g hostname pairs = let
        Just qop   = L.lookup "qop" pairs
        Just nonce = L.lookup "nonce" pairs
        uname_     = Text.encodeUtf8 uname
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
    -- Failure element pickler.
    failurePickle :: PU [Node] SaslFailure
    failurePickle = xpWrap
        (\(txt, (failure, _, _)) -> SaslFailure failure txt)
        (\(SaslFailure failure txt) -> (txt,(failure,(),())))
        (xpElemNodes
            "{urn:ietf:params:xml:ns:xmpp-sasl}failure"
            (xp2Tuple
                 (xpOption $ xpElem
                      "{urn:ietf:params:xml:ns:xmpp-sasl}text"
                      xpLangTag
                      (xpContent xpId))
            (xpElemByNamespace
                 "urn:ietf:params:xml:ns:xmpp-sasl"
                 xpPrim
                 (xpUnit)
                 (xpUnit))))
    -- Challenge element pickler.
    challengePickle :: PU [Node] Text.Text
    challengePickle = xpElemNodes "{urn:ietf:params:xml:ns:xmpp-sasl}challenge"
                          (xpIsolate $ xpContent xpId)