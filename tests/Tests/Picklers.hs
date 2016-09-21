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

prop_xpStreamStanza_invertible :: Either StreamErrorInfo Stanza -> Bool
prop_xpStreamStanza_invertible         = tpsi xpStreamStanza
prop_xpStanza_invertible :: Stanza -> Bool
prop_xpStanza_invertible               = tpsi xpStanza
prop_xpMessage_invertible :: Message -> Bool
prop_xpMessage_invertible              = tpsi xpMessage
prop_xpPresence_invertible             = tpsi xpPresence
prop_xpPresence_invertible :: Presence -> Bool
prop_xpIQRequest_invertible            = tpsi xpIQRequest
prop_xpIQRequest_invertible :: IQRequest -> Bool
prop_xpIQResult_invertible             = tpsi xpIQResult
prop_xpIQResult_invertible :: IQResult -> Bool
prop_xpErrorCondition_invertible       = tpsi xpStanzaErrorCondition
prop_xpErrorCondition_invertible :: StanzaErrorCondition -> Bool
prop_xpStanzaError_invertible          = tpsi xpStanzaError
prop_xpStanzaError_invertible :: StanzaError -> Bool
prop_xpMessageError_invertible         = tpsi xpMessageError
prop_xpMessageError_invertible :: MessageError -> Bool
prop_xpPresenceError_invertible        = tpsi xpPresenceError
prop_xpPresenceError_invertible :: PresenceError -> Bool
prop_xpIQError_invertible              = tpsi xpIQError
prop_xpIQError_invertible :: IQError -> Bool
prop_xpStreamError_invertible          = tpsi xpStreamError
prop_xpStreamError_invertible :: StreamErrorInfo -> Bool
prop_xpLangTag_invertible              = tpsi xpLangTag
prop_xpLangTag_invertible :: Maybe LangTag -> Bool
prop_xpLang_invertible                 = tpsi xpLang
prop_xpLang_invertible :: LangTag -> Bool
prop_xpStream_invertible               = tpsi xpStream
prop_xpStream_invertible :: ( Text
                           , Maybe Jid
                           , Maybe Jid
                           , Maybe Text
                           , Maybe LangTag )
                           -> Bool
prop_xpJid_invertible                  = tpsi xpJid
prop_xpJid_invertible :: Jid -> Bool
prop_xpIQRequestType_invertible        = tpsi xpIQRequestType
prop_xpIQRequestType_invertible :: IQRequestType -> Bool
prop_xpMessageType_invertible          = tpsi xpMessageType
prop_xpMessageType_invertible :: MessageType -> Bool
prop_xpPresenceType_invertible         = tpsi xpPresenceType
prop_xpPresenceType_invertible :: PresenceType -> Bool
prop_xpStanzaErrorType_invertible      = tpsi xpStanzaErrorType
prop_xpStanzaErrorType_invertible :: StanzaErrorType -> Bool
prop_xpStanzaErrorCondition_invertible = tpsi xpStanzaErrorCondition
prop_xpStanzaErrorCondition_invertible :: StanzaErrorCondition -> Bool
prop_xpStreamErrorCondition_invertible = tpsi xpStreamErrorCondition
prop_xpStreamErrorCondition_invertible :: StreamErrorCondition -> Bool
-- prop_xpStreamFeatures_invertible = testPicklerInvertible xpStreamFeatures

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
