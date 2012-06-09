{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.Xmpp.Sasl.Common where

import           Network.Xmpp.Types

import           Control.Applicative ((<$>))
import           Control.Monad.Error
import           Control.Monad.State.Class

import qualified Data.Attoparsec.ByteString.Char8 as AP
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as B64
import           Data.Maybe (fromMaybe)
import           Data.Maybe (maybeToList)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import           Data.XML.Pickle
import           Data.XML.Types
import           Data.Word (Word8)

import           Network.Xmpp.Monad
import           Network.Xmpp.Pickle
import           Network.Xmpp.Sasl.Types

import qualified System.Random as Random

data SaslElement = SaslSuccess   (Maybe Text.Text)
                 | SaslChallenge (Maybe Text.Text)

--makeNonce :: SaslM BS.ByteString
makeNonce :: IO BS.ByteString
makeNonce = do
    g <- liftIO Random.newStdGen
    return $ B64.encode . BS.pack . map toWord8 . take 15 $ Random.randoms g
  where
    toWord8 :: Int -> Word8
    toWord8 x = fromIntegral x :: Word8

-- The <auth xmlns='urn:ietf:params:xml:ns:xmpp-sasl'/> element, with an
-- optional round-trip value.
saslInitE :: Text.Text -> Maybe Text.Text -> Element
saslInitE mechanism rt =
    Element "{urn:ietf:params:xml:ns:xmpp-sasl}auth"
        [("mechanism", [ContentText mechanism])]
        (maybeToList $ NodeContent . ContentText <$> rt)

-- SASL response with text payload.
saslResponseE :: Maybe Text.Text -> Element
saslResponseE resp =
    Element "{urn:ietf:params:xml:ns:xmpp-sasl}response"
    []
    (maybeToList $ NodeContent . ContentText <$> resp)

xpSuccess :: PU [Node] (Maybe Text.Text)
xpSuccess = xpElemNodes "{urn:ietf:params:xml:ns:xmpp-sasl}success"
              (xpOption $ xpContent xpId)

-- Parses the incoming SASL data to a mapped list of pairs.
pairs :: BS.ByteString -> Either String Pairs
pairs = AP.parseOnly . flip AP.sepBy1 (void $ AP.char ',') $ do
    AP.skipSpace
    name <- AP.takeWhile1 (/= '=')
    _ <- AP.char '='
    quote <- ((AP.char '"' >> return True) `mplus` return False)
    content <- AP.takeWhile1 (AP.notInClass [',', '"'])
    when quote . void $ AP.char '"'
    return (name, content)

-- Failure element pickler.
xpFailure :: PU [Node] SaslFailure
xpFailure = xpWrap
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
xpChallenge :: PU [Node] (Maybe Text.Text)
xpChallenge = xpElemNodes "{urn:ietf:params:xml:ns:xmpp-sasl}challenge"
                      (xpOption $ xpContent xpId)

-- | pickler for SaslElement
xpSaslElement :: PU [Node] SaslElement
xpSaslElement = xpAlt saslSel
                [ xpWrap SaslSuccess   (\(SaslSuccess x)   -> x) xpSuccess
                , xpWrap SaslChallenge (\(SaslChallenge c) -> c) xpChallenge
                ]
  where
    saslSel (SaslSuccess   _) = 0
    saslSel (SaslChallenge _) = 1

-- | Add quotationmarks around a byte string
quote :: BS.ByteString -> BS.ByteString
quote x = BS.concat ["\"",x,"\""]

saslInit :: Text.Text -> Maybe BS.ByteString -> SaslM Bool
saslInit mechanism payload = lift . pushElement . saslInitE mechanism $
                               Text.decodeUtf8 . B64.encode <$> payload

-- | Pull the next element
pullSaslElement :: SaslM SaslElement
pullSaslElement = do
    el <- lift $ pullPickle (xpEither xpFailure xpSaslElement)
    case el of
        Left e ->throwError $ AuthSaslFailure e
        Right r -> return r

-- | Pull the next element, checking that it is a challenge
pullChallenge :: SaslM (Maybe BS.ByteString)
pullChallenge = do
  e <- pullSaslElement
  case e of
      SaslChallenge Nothing -> return Nothing
      SaslChallenge (Just scb64)
          | Right sc <- B64.decode . Text.encodeUtf8 $ scb64
             -> return $ Just sc
      _ -> throwError AuthChallengeError

-- | Extract value from Just, failing with AuthChallengeError on Nothing
saslFromJust :: Maybe a -> SaslM a
saslFromJust Nothing = throwError $ AuthChallengeError
saslFromJust (Just d) = return d

-- | Pull the next element and check that it is success
pullSuccess :: SaslM (Maybe Text.Text)
pullSuccess = do
    e <- pullSaslElement
    case e of
        SaslSuccess x -> return x
        _ -> throwError $ AuthXmlError

-- | Pull the next element. When it's success, return it's payload.
-- If it's a challenge, send an empty response and pull success
pullFinalMessage :: SaslM (Maybe Text.Text)
pullFinalMessage = do
    challenge2 <- pullSaslElement
    case challenge2 of
        SaslSuccess   x -> return x
        SaslChallenge x -> do
            _b <- respond Nothing
            pullSuccess
            return x

-- | Extract p=q pairs from a challenge
toPairs :: BS.ByteString -> SaslM Pairs
toPairs ctext = case pairs ctext of
    Left _e -> throwError AuthChallengeError
    Right r -> return r

-- | Send a SASL response element. The content will be base64-encoded  for you
respond :: Maybe BS.ByteString -> SaslM Bool
respond = lift . pushElement . saslResponseE .
              fmap (Text.decodeUtf8 . B64.encode)

