{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Tests.Picklers where

import Tests.Arbitrary ()
import Data.XML.Pickle
import Network.Xmpp.Marshal
import Network.Xmpp.Types
import Test.Tasty
import Test.Tasty.TH
import Test.Tasty.QuickCheck

import Data.XML.Types

testPicklerInvertible :: Eq a => PU t a -> a -> Bool
testPicklerInvertible p = \x -> case unpickle p (pickle p x) of
    Left _ -> False
    Right x' -> x == x'

testPickler p x = case unpickle p (pickle p x) of
    Left e -> putStrLn $ ppUnpickleError e
    Right r -> putStrLn "OK."

prop_errorConditionPicklerInvertible :: StanzaErrorCondition -> Bool
prop_errorConditionPicklerInvertible = testPicklerInvertible xpErrorCondition

prop_stanzaErrorPicklerInvertible :: StanzaError -> Bool
prop_stanzaErrorPicklerInvertible = testPicklerInvertible xpStanzaError

prop_messagePicklerInvertible :: Message -> Bool
prop_messagePicklerInvertible = testPicklerInvertible xpMessage

prop_messageErrorPicklerInvertible :: MessageError -> Bool
prop_messageErrorPicklerInvertible = testPicklerInvertible xpMessageError

prop_presencePicklerInvertible :: Presence -> Bool
prop_presencePicklerInvertible = testPicklerInvertible xpPresence

prop_presenceErrorPicklerInvertible :: PresenceError -> Bool
prop_presenceErrorPicklerInvertible = testPicklerInvertible xpPresenceError

prop_iqRequestPicklerInvertible :: IQRequest -> Bool
prop_iqRequestPicklerInvertible = testPicklerInvertible xpIQRequest

prop_iqResultPicklerInvertible :: IQResult -> Bool
prop_iqResultPicklerInvertible = testPicklerInvertible xpIQResult

prop_iqErrorPicklerInvertible :: IQError -> Bool
prop_iqErrorPicklerInvertible = testPicklerInvertible xpIQError

prop_langTagPicklerInvertible :: Maybe LangTag -> Bool
prop_langTagPicklerInvertible = testPicklerInvertible xpLangTag

prop_langPicklerInvertible :: LangTag -> Bool
prop_langPicklerInvertible = testPicklerInvertible xpLang

picklerTests :: TestTree
picklerTests = $testGroupGenerator

bad1 = StanzaError { stanzaErrorType = Cancel
                  , stanzaErrorCondition = Forbidden
                  , stanzaErrorText = Just $ (Just $ LangTag "v" [], "")
                  , stanzaErrorApplicationSpecificCondition =
                      Just (Element {elementName =
                                          Name { nameLocalName = "\231"
                                               , nameNamespace = Nothing
                                               , namePrefix = Nothing}
                                    , elementAttributes = []
                                    , elementNodes = []
                                    })
                  }

bad2StanzaError = StanzaError { stanzaErrorType = Continue
                              , stanzaErrorCondition = NotAllowed
                              , stanzaErrorText = Just (Just $ parseLangTag "W-o","\f")
                              , stanzaErrorApplicationSpecificCondition =
                                  Just (Element {elementName =
                                                      Name { nameLocalName = "\8204"
                                                           , nameNamespace = Nothing
                                                           , namePrefix = Just "\8417A"}
                                                , elementAttributes = []
                                                , elementNodes = []})}

bad2 = MessageError { messageErrorID = Just ""
                    , messageErrorFrom = Just $ parseJid "a@y/\177"
                    , messageErrorTo = Just $ parseJid "\250@7"
                    , messageErrorLangTag = Nothing
                    , messageErrorStanzaError = bad2StanzaError
                    , messageErrorPayload =
                            [Element {elementName =
                                           Name { nameLocalName = "\12226C"
                                                , nameNamespace = Nothing
                                                , namePrefix = Nothing}
                                     , elementAttributes = []
                                     , elementNodes = []}]}
