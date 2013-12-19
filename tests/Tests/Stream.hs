{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Tests.Stream where

import           Data.Conduit
import qualified Data.Conduit.List as CL
import           Data.XML.Types
import           Test.Hspec
import           Test.Tasty.TH
import           Test.Tasty
import           Test.Tasty.Hspec

import           Network.Xmpp.Stream

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

case_ThrowOutJunk = do
    it "drops everything but EvenBeginElement" $ do
        res <- CL.sourceList junk $$ throwOutJunk >> await
        res `shouldBe` Nothing
    it "keeps everything after (and including) EvenBeginElement" $ do
        res <- CL.sourceList (junk ++ [beginElem] ++ junk)
                             $$ throwOutJunk >> CL.consume
        res `shouldBe` (beginElem : junk)

testStreams :: TestTree
testStreams = $testGroupGenerator
