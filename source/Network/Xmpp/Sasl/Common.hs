{-# LANGUAGE OverloadedStrings #-}

module Network.Xmpp.Sasl.Common where

import           Network.Xmpp.Types

import           Control.Applicative ((<$>))
import           Control.Monad.Error

import qualified Data.Attoparsec.ByteString.Char8 as AP
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as B64
import           Data.Maybe (fromMaybe)
import           Data.Maybe (maybeToList)
import           Data.Text
import qualified Data.Text.Encoding as Text
import           Data.XML.Pickle
import           Data.XML.Types

import           Network.Xmpp.Monad
import           Network.Xmpp.Pickle
import           Network.Xmpp.Sasl.Types

data SaslElement = SaslSuccess
                 | SaslChallenge (Maybe Text)

-- The <auth xmlns='urn:ietf:params:xml:ns:xmpp-sasl'/> element, with an
-- optional round-trip value.
saslInitE :: Text -> Maybe Text -> Element
saslInitE mechanism rt =
    Element "{urn:ietf:params:xml:ns:xmpp-sasl}auth"
        [("mechanism", [ContentText mechanism])]
        (maybeToList $ NodeContent . ContentText <$> rt)

-- SASL response with text payload.
saslResponseE :: Maybe Text -> Element
saslResponseE resp =
    Element "{urn:ietf:params:xml:ns:xmpp-sasl}response"
    []
    (maybeToList $ NodeContent . ContentText <$> resp)

xpSuccess :: PU [Node] ()
xpSuccess = xpElemBlank "{urn:ietf:params:xml:ns:xmpp-sasl}success"

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
xpChallenge :: PU [Node] (Maybe Text)
xpChallenge = xpElemNodes "{urn:ietf:params:xml:ns:xmpp-sasl}challenge"
                      (xpOption $ xpContent xpId)

xpSaslElement = xpAlt saslSel
                [ xpWrap (const SaslSuccess) (const ())          xpSuccess
                , xpWrap SaslChallenge (\(SaslChallenge c) -> c) xpChallenge
                ]
  where
    saslSel SaslSuccess       = 0
    saslSel (SaslChallenge _) = 1

quote :: BS.ByteString -> BS.ByteString
quote x = BS.concat ["\"",x,"\""]

saslInit :: Text -> Maybe Text -> SaslM Bool
saslInit mechanism payload = lift . pushElement $ saslInitE mechanism payload

pullSaslElement :: SaslM SaslElement
pullSaslElement = do
    el <- lift $ pullPickle (xpEither xpFailure xpSaslElement)
    case el of
        Left e ->throwError $ AuthSaslFailure e
        Right r -> return r

pullChallenge :: SaslM (Maybe Text)
pullChallenge = do
  e <- pullSaslElement
  case e of
      SaslChallenge sc -> return sc
      _ -> throwError AuthChallengeError

saslFromJust :: Maybe a -> SaslM a
saslFromJust Nothing = throwError $ AuthChallengeError
saslFromJust (Just d) = return d

pullSuccess :: SaslM ()
pullSuccess = do
    e <- pullSaslElement
    case e of
        SaslSuccess -> return ()
        _ -> throwError $ AuthXmlError

toPairs :: Text -> SaslM Pairs
toPairs ctext = case pairs <=< B64.decode . Text.encodeUtf8 $ ctext of
    Left _e -> throwError AuthChallengeError
    Right r -> return r

respond :: Maybe Text -> SaslM Bool
respond = lift . pushElement . saslResponseE

