{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Tests.Echo where

import           Control.Concurrent.Async
import           Control.Concurrent.STM
import           Control.Lens
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Maybe
import qualified Data.Text as Text
import           Data.XML.Pickle
import           Data.XML.Types
import           Network.Xmpp
import           Network.Xmpp.Marshal (pickleElem, unpickleElem)
import           System.Log.Logger
import           System.Timeout
import           Test.Hspec
import           Tests.Common

testNS :: Text.Text
testNS = "xmpp:library:test"

testName :: Text.Text -> Name
testName name = Name name (Just testNS) Nothing

data Payload = Payload
               { _payloadWantError    :: Bool
               , _payloadWantTimeout  :: Bool
               , _payloadText         :: Text.Text
               } deriving (Eq, Show)

makeLenses ''Payload

defaultPL :: Payload
defaultPL = Payload False False "ping"

-- | Automatically accept all subscription requests from other entities
autoAccept :: Session -> IO ()
autoAccept sess = forever $ do
  st <- waitForPresence ((== Subscribe ) . presenceType ) sess
  case presenceFrom st of
      Just fr -> do
          _ <- sendPresence (presenceSubscribed fr) sess
          return ()
      Nothing -> return () -- this shouldn't happen

payloadP :: PU [Node] Payload
payloadP = xpWrap (\((err, tout) , msg) -> Payload err tout msg)
                  (\(Payload err tout msg) ->
                       ((err, tout) , msg)) $
                   xpElem (Name "request" (Just testNS) Nothing)
                      (xp2Tuple
                        (xpAttr "want-error" xpPrim)
                        (xpAttr "want-timeout" xpPrim)
                      )
                      (xpElemNodes (Name "message" (Just testNS) Nothing)
                          (xpContent xpId))

iqResponder :: Session -> IO b
iqResponder sess = do
  chan' <- listenIQChan Set testNS sess
  chan <- case chan' of
      Left _ -> assertionFailed  "Channel was already taken"
      Right c -> return c
  forever . void . runMaybeT $ do
    next <- liftIO . atomically $ readTChan chan
    let Right pld = unpickleElem payloadP . iqRequestPayload $
                          iqRequestBody next
    when (pld^.payloadWantTimeout) mzero
    let answerPayload = Payload False False (Text.reverse $ pld^.payloadText)
    let answerBody = pickleElem payloadP answerPayload
    let answer = case pld^.payloadWantError of
            True -> Left $ mkStanzaError UndefinedCondition
            False -> Right $ Just answerBody
    liftIO $ answerIQ next answer

mirror :: Session -> IO b
mirror sess = forever $ do
    msg <- getMessage sess
    case (answerMessage msg (view payload msg)) of
        Nothing -> return ()
        Just answer -> void $ sendMessage answer sess

resp :: Session -> IO b
resp sess =
    withAsync (iqResponder sess) $ \_ ->
    mirror sess


---------------------------------------
-- Tests ------------------------------
---------------------------------------

test_messagePing :: Session -> IO ()
test_messagePing sess = do
    infoM "Pontarius.Xmpp.Tests" "RUNNING message ping test"
    let el = [Element (testName "ping") [] []]
    _ <- sendMessage (message{ messagePayload = el
                             , messageTo = Just responder
                             }) sess
    mbMessage <- timeout timeoutDuration (getMessage sess)
    case mbMessage of
        Nothing -> assertionFailed $ "ping: No answer within "
                   ++ show (timeoutDuration `div` 1000000 :: Integer) ++ "seconds"
        Just msg -> view payload msg `shouldBe` el
    infoM "Pontarius.Xmpp.Tests" "SUCCESS message ping test"

test_IQ :: Payload
        -> Session
        -> IO (Either IQSendError IQResponse)
test_IQ pl sess = do
    let el = pickleElem payloadP pl
    sendIQ' (Just timeoutDuration) (Just responder) Set Nothing el sess

test_IQPing :: Session -> IO ()
test_IQPing sess = do
    infoM "Pontarius.Xmpp.Tests" "RUNNING IQ ping test"
    response <- test_IQ defaultPL sess
    case response of
        Left e -> assertionFailed $ "test_IQPing: " ++ show e
        Right (IQResponseError e) -> assertionFailed $ "iqPing: " ++ show e
        Right (IQResponseResult r) -> do
            case view payload r of
                Nothing -> assertionFailed "test_IQPing: no payload"
                Just pl' -> case unpickleElem payloadP pl' of
                    Left e -> assertionFailed $ "test_IQPing: unpickling\
                                    \returned unpickleError" ++ show e
                    Right pl -> pl `shouldBe` Payload False False "gnip"
    infoM "Pontarius.Xmpp.Tests" "SUCCESS IQ ping test"

test_IQError :: Session -> IO ()
test_IQError sess = do
    infoM "Pontarius.Xmpp.Tests" "RUNNING IQ Error test"
    response <- test_IQ (defaultPL & payloadWantError .~ True) sess
    case response of
        Left e -> assertionFailed $ "iqPing: " ++ show e
        Right (IQResponseError e) -> e^.stanzaError.stanzaErrorConditionL
                                       `shouldBe` UndefinedCondition
        Right (IQResponseResult r) -> assertionFailed $ "test_IQError:\
                                          \Expected IQ error but got" ++ show r
    infoM "Pontarius.Xmpp.Tests" "SUCCESS IQ Error test"

test_IQTimeout :: Session -> IO ()
test_IQTimeout sess = do
    infoM "Pontarius.Xmpp.Tests" "RUNNING IQ Timeout test"
    response <- test_IQ (defaultPL & payloadWantTimeout .~ True) sess
    case response of
        Left IQTimeOut -> return ()
        err -> assertionFailed $ "test_IQTimeout: Expected timeout but got"
                                  ++ show err
    infoM "Pontarius.Xmpp.Tests" "SUCCESS IQ Timeout test"


tests :: Session -> IO ()
tests sess = do
    infoM "Pontarius.Xmpp.Tests" "RUNNING test barrage"
    test_messagePing sess
    test_IQPing sess
    test_IQError sess
    test_IQTimeout sess
    infoM "Pontarius.Xmpp.Tests" "SUCCESS test barrage"

test :: IO ()
test = do
    updateGlobalLogger "Pontarius.Xmpp" $ setLevel INFO
    prepareThreads resp tests
