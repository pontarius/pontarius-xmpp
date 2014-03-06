{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE LambdaCase #-}


module Run.Payload where

import           Control.Monad
import           Control.Monad.STM
import qualified Data.Text as Text
import           Data.XML.Pickle
import           Data.XML.Types
import           Network.Xmpp
import           Network.Xmpp.Internal
import           System.Log.Logger
import           Test.HUnit hiding (Node)
import           Test.Hspec.Expectations

data Payload = Payload
               { payloadCounter :: !Int
               , ignoreFlag     :: !Bool
               , errorFlag      :: !Bool
               , payloadText    :: !Text.Text
               } deriving (Eq, Show)

testNS :: Text.Text
testNS = "xmpp:library:test"

payloadP :: PU [Node] Payload
payloadP = xpWrap (\((counter,iFlag, eFlag) , message)
                      -> Payload counter iFlag eFlag message)
                  (\(Payload counter iFlag eFlag message)
                      ->((counter,iFlag, eFlag) , message)) $
                   xpElem (Name "request" (Just testNS) Nothing)
                      (xp3Tuple
                        (xpAttr "counter" xpPrim)
                        (xpAttr "ignoreFlag" xpPrim)
                        (xpAttr "errorFlag" xpPrim)
                      )
                      (xpElemNodes (Name "message" (Just testNS) Nothing)
                          (xpContent xpId))

invertPayload :: Payload -> Payload
invertPayload (Payload count _iFlag _eFlag message) =
    Payload (count + 1) False False (Text.reverse message)

iqResponder :: Session -> IO ()
iqResponder context = do
    chan' <- listenIQ Set testNS context
    chan <- case chan' of
        Left _ -> do
            assertFailure "Channel was already taken"
            undefined
        Right c -> return c
    forever $ do
        next <- atomically $ chan
        let Right payload = unpickleElem payloadP . iqRequestPayload $
                              iqRequestBody next
        let answerPayload = invertPayload payload
        let answerBody = pickleElem payloadP answerPayload
        unless (ignoreFlag payload) . void $
            case errorFlag payload of
                False -> answerIQ next (Right $ Just answerBody) []
                True -> answerIQ next (Left $ mkStanzaError NotAcceptable) []

testString :: Text.Text
testString = "abc ÄÖ>"

testPayload :: Jid -> Session -> IO ()
testPayload them session = do
    infoM "Pontarius.Xmpp" "Testing IQ send/receive"
    let pl1 = Payload 1 False False testString
        body1 = pickleElem payloadP pl1
    resp <- sendIQ' (Just 3000000) (Just them) Set Nothing body1 [] session

    case resp of
        Left e -> assertFailure $ "Could not send pl1" ++ show e
        Right (IQResponseError e) ->
            assertFailure $ "Unexpected IQ error" ++ show e
        Right (IQResponseResult IQResult{iqResultPayload = Just pl}) -> do
            case unpickleElem payloadP pl of
                Left e -> assertFailure $ "Error unpickling response p1"
                                           ++ ppUnpickleError e
                Right r -> do
                    payloadCounter r `shouldBe` 2
                    payloadText r `shouldBe` Text.reverse testString
        Right (IQResponseResult _) ->
            assertFailure "IQ result didn't contain payload"
    infoM "Pontarius.Xmpp" "Done testing IQ send/receive"
    ----------------------
    -- Timeout test
    ----------------------
    let pl2 = Payload 2 True False testString
        body2 = pickleElem payloadP pl2
    infoM "Pontarius.Xmpp" "Testing timeout"
    resp <- sendIQ' (Just 1000000) (Just them) Set Nothing body2 [] session
    case resp of
        Left IQTimeOut -> return ()
        Left e -> assertFailure $ "Unexpected send error" ++ show e
        Right r -> assertFailure $ "Unexpected IQ answer" ++ show r
    infoM "Pontarius.Xmpp" "IQ timed out (as expected)"
    ----------------------
    -- Error test
    ----------------------
    infoM "Pontarius.Xmpp" "Testing IQ error"
    let pl3 = Payload 3 False True testString
        body3 = pickleElem payloadP pl3
    resp <- sendIQ' (Just 3000000) (Just them) Set Nothing body3 [] session
    case resp of
        Left e -> assertFailure $ "Unexpected send error" ++ show e
        Right (IQResponseError e) ->
             stanzaErrorCondition (iqErrorStanzaError e) `shouldBe` NotAcceptable

        Right r -> assertFailure $ "Received unexpected IQ response" ++ show r
    infoM "Pontarius.Xmpp" "Received expected error"
