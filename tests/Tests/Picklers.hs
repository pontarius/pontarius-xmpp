{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Tests.Picklers where

import Tests.Arbitrary ()
import Data.XML.Pickle
import Network.Xmpp.Internal
import Test.Tasty
import Test.Tasty.TH
import Test.Tasty.QuickCheck
import Data.Text (Text)

import Data.XML.Types

-- | Test Pickler self-inverse: Check that unpickling after pickling gives the
-- original value
tpsi :: Eq a => PU t a -> a -> Bool
tpsi p = \x -> case unpickle p (pickle p x) of
    Left _ -> False
    Right x' -> x == x'

testPickler :: PU t a -> a -> IO ()
testPickler p x = case unpickle p (pickle p x) of
    Left e -> putStrLn $ ppUnpickleError e
    Right _ -> putStrLn "OK."

prop_xpStreamStanza_invertibe :: Either StreamErrorInfo Stanza -> Bool
prop_xpStreamStanza_invertibe         = tpsi xpStreamStanza
prop_xpStanza_invertibe :: Stanza -> Bool
prop_xpStanza_invertibe               = tpsi xpStanza
prop_xpMessage_invertibe :: Message -> Bool
prop_xpMessage_invertibe              = tpsi xpMessage
prop_xpPresence_invertibe             = tpsi xpPresence
prop_xpPresence_invertibe :: Presence -> Bool
prop_xpIQRequest_invertibe            = tpsi xpIQRequest
prop_xpIQRequest_invertibe :: IQRequest -> Bool
prop_xpIQResult_invertibe             = tpsi xpIQResult
prop_xpIQResult_invertibe :: IQResult -> Bool
prop_xpErrorCondition_invertibe       = tpsi xpStanzaErrorCondition
prop_xpErrorCondition_invertibe :: StanzaErrorCondition -> Bool
prop_xpStanzaError_invertibe          = tpsi xpStanzaError
prop_xpStanzaError_invertibe :: StanzaError -> Bool
prop_xpMessageError_invertibe         = tpsi xpMessageError
prop_xpMessageError_invertibe :: MessageError -> Bool
prop_xpPresenceError_invertibe        = tpsi xpPresenceError
prop_xpPresenceError_invertibe :: PresenceError -> Bool
prop_xpIQError_invertibe              = tpsi xpIQError
prop_xpIQError_invertibe :: IQError -> Bool
prop_xpStreamError_invertibe          = tpsi xpStreamError
prop_xpStreamError_invertibe :: StreamErrorInfo -> Bool
prop_xpLangTag_invertibe              = tpsi xpLangTag
prop_xpLangTag_invertibe :: Maybe LangTag -> Bool
prop_xpLang_invertibe                 = tpsi xpLang
prop_xpLang_invertibe :: LangTag -> Bool
prop_xpStream_invertibe               = tpsi xpStream
prop_xpStream_invertibe :: ( Text
                           , Maybe Jid
                           , Maybe Jid
                           , Maybe Text
                           , Maybe LangTag )
                           -> Bool
prop_xpJid_invertibe                  = tpsi xpJid
prop_xpJid_invertibe :: Jid -> Bool
prop_xpIQRequestType_invertibe        = tpsi xpIQRequestType
prop_xpIQRequestType_invertibe :: IQRequestType -> Bool
prop_xpMessageType_invertibe          = tpsi xpMessageType
prop_xpMessageType_invertibe :: MessageType -> Bool
prop_xpPresenceType_invertibe         = tpsi xpPresenceType
prop_xpPresenceType_invertibe :: PresenceType -> Bool
prop_xpStanzaErrorType_invertibe      = tpsi xpStanzaErrorType
prop_xpStanzaErrorType_invertibe :: StanzaErrorType -> Bool
prop_xpStanzaErrorCondition_invertibe = tpsi xpStanzaErrorCondition
prop_xpStanzaErrorCondition_invertibe :: StanzaErrorCondition -> Bool
prop_xpStreamErrorCondition_invertibe = tpsi xpStreamErrorCondition
prop_xpStreamErrorCondition_invertibe :: StreamErrorCondition -> Bool
-- prop_xpStreamFeatures_invertibe = testPicklerInvertible xpStreamFeatures

picklerTests :: TestTree
picklerTests = $testGroupGenerator

bad = StreamErrorInfo { errorCondition = StreamInvalidFrom
                      , errorText = Just (Nothing,"")
                      , errorXml = Just (
                          Element { elementName =
                                         Name { nameLocalName = "\65044"
                                              , nameNamespace = Just "\14139"
                                              , namePrefix = Just "\651"}
                                  , elementAttributes = []
                                  , elementNodes = []})}
