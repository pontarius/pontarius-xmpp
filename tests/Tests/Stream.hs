{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Tests.Stream where

import           Control.Monad.Trans
import           Data.Conduit
import qualified Data.Conduit.List as CL
import           Data.XML.Types
import           Test.Hspec
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.Hspec
import           Test.Tasty.TH

import           Network.Xmpp.Internal

junk = [ EventBeginDocument
       , EventEndDocument
       , EventBeginDoctype "" Nothing
       , EventEndDoctype
       , EventInstruction $ Instruction "" ""
--       , EventBeginElement Name [(Name, [Content])]
       , EventEndElement "foo"
       , EventContent $ ContentText ""
       , EventComment ""
       , EventCDATA ""
       ]

beginElem = EventBeginElement "foo" []

case_ThrowOutJunk = hspec . describe "throwOutJunk" $ do
    it "drops everything but EvenBeginElement" $ do
        res <- CL.sourceList junk $$ throwOutJunk >> await
        res `shouldBe` Nothing
    it "keeps everything after (and including) EvenBeginElement" $ do
        res <- CL.sourceList (junk ++ [beginElem] ++ junk)
                             $$ throwOutJunk >> CL.consume
        res `shouldBe` (beginElem : junk)

case_LogInput = hspec . describe "logInput" $ do
    it "Can handle split UTF8 codepoints" $ do
        res <- CL.sourceList ["\209","\136"] $= logInput $$ CL.consume
        res `shouldBe` ["\209","\136"]

streamTests :: TestTree
streamTests = $testGroupGenerator
