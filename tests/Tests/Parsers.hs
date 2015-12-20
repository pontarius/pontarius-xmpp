{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Tests.Parsers where

import Control.Applicative ((<$>))
import Network.Xmpp.Internal
import Test.Hspec
import Test.Tasty.QuickCheck
import Test.Tasty
import Test.Tasty.TH
import Test.Tasty.HUnit

import Tests.Arbitrary ()

case_JidFromText :: Assertion
case_JidFromText = hspec . describe "jidFromText" $ do
    it "parses a full JID" $ jidFromText "foo@bar.com/quux"
                             `shouldBe` Just (Jid (Just "foo")
                                          "bar.com"
                                          (Just "quux"))
    it "parses a bare JID" $ jidFromText "foo@bar.com"
                             `shouldBe` Just (Jid (Just "foo")
                                          "bar.com"
                                          Nothing)
    it "parses a domain" $ jidFromText "bar.com"
                             `shouldBe` Just (Jid Nothing
                                          "bar.com"
                                          Nothing)
    it "parses domain with resource" $ jidFromText "bar.com/quux"
                             `shouldBe` Just (Jid Nothing
                                          "bar.com"
                                          (Just "quux"))
    it "rejects multiple '@'" $  shouldReject "foo@bar@baz"
    it "parses multiple '/'" $  jidFromText "foo/bar/baz"
                                `shouldBe`
                                (Just (Jid Nothing "foo" (Just "bar/baz")))
    it "parses multiple '/' after '@'" $ jidFromText  "quux@foo/bar/baz"
                               `shouldBe`
                               (Just (Jid (Just "quux") "foo" (Just "bar/baz")))
    it "parses '@' after '/'" $ jidFromText "foo/bar@baz"
                               `shouldBe`
                               (Just (Jid Nothing "foo" (Just "bar@baz")))
    it "rejects empty local part" $ shouldReject "@bar/baz"
    it "rejects empty resource part" $ shouldReject "foo@bar/"
    it "rejects empty domain part" $ shouldReject "foo@/baz"
  where shouldReject jid = jidFromText jid `shouldBe` Nothing

prop_jidFromText_rightInverse :: Jid -> Bool
prop_jidFromText_rightInverse j = let jidText = jidToText j in
        (jidToText <$> jidFromText jidText) == Just jidText

prop_jidFromText_leftInverse :: Jid -> Bool
prop_jidFromText_leftInverse jid = (jidFromText $ jidToText jid) == Just jid


case_LangTagParser :: Assertion
case_LangTagParser = hspec . describe "langTagFromText" $
                    it "has some properties" $ pendingWith "Check requirements"

parserTests :: TestTree
parserTests = $testGroupGenerator
