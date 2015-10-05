{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE LambdaCase #-}

module Run.SendReceive where

import           Control.Applicative
import           Control.Concurrent
import           Control.Monad
import qualified Data.Configurator as Conf
import qualified Data.Configurator.Types as Conf
import           Data.Maybe
import qualified Data.Text as Text
import           Network
import           Network.Xmpp
import           System.Exit
import           System.Log.Logger
import           System.Timeout
import           Test.HUnit
import           Test.Hspec.Expectations

import           Run.Payload
import           Run.Config

xmppConfig :: ConnectionDetails -> SessionConfiguration
xmppConfig det = def{sessionStreamConfiguration
                          = def{connectionDetails = det}
                    , onConnectionClosed = \sess _ -> do
                          _ <- reconnect' sess
                          _ <- sendPresence presenceOnline sess
                          return ()
                    }

-- | reflect messages to their origin
reflect :: Session -> IO b
reflect sess = forever $ do
    m <- getMessage sess
    case answerMessage m (messagePayload m) of
        Nothing -> return ()
        Just am ->
            void $ sendMessage am{messageAttributes = messageAttributes m} sess

testAttributes = [( "{org.pontarius.xmpp.test}testattr"
                  , "testvalue  12321 åäü>"
                  )]

run :: IO ()
run = void $ do
    conf <- loadConfig
    uname1 <- Conf.require conf "xmpp.user1"
    pwd1 <- Conf.require conf "xmpp.password1"
    uname2 <- Conf.require conf "xmpp.user1"
    pwd2 <- Conf.require conf "xmpp.password1"
    realm <- Conf.require conf "xmpp.realm"
    server <- Conf.lookup conf "xmpp.server"
    port <- Conf.lookup conf "xmpp.port" :: IO (Maybe Integer)
    let conDetails = case server of
            Nothing -> UseRealm
            Just srv -> case port of
                Nothing -> UseSrv srv
                Just p -> UseHost srv (fromIntegral p)
    _ <- configuredLoglevel conf
    mbSess1 <- session realm (simpleAuth uname1 pwd1)
                                ((xmppConfig conDetails))
    sess1 <- case mbSess1 of
        Left e -> do
            assertFailure $ "session 1 could not be initialized" ++ show e
            exitFailure
        Right r -> return r
    mbSess2 <- session realm (simpleAuth uname2 pwd2)
                                ((xmppConfig conDetails))
    sess2 <- case mbSess2 of
        Left e -> do
            assertFailure $ "session 2 could not be initialized" ++ show e
            exitFailure
        Right r -> return r
    Just jid1 <- getJid sess1
    Just jid2 <- getJid sess2
    _ <- sendPresence presenceOnline sess1
    _ <- forkIO $ reflect sess1
    forkIO $ iqResponder sess1
    _ <- sendPresence presenceOnline sess2
    -- check message responsiveness
    infoM "Pontarius.Xmpp" "Running message mirror"
    sendMessage message{ messageTo = Just jid1
                       , messageAttributes = testAttributes
                       } sess2
    resp <- timeout 3000000 $ waitForMessage (\m -> messageFrom m == Just jid1)
                                             sess2
    case resp of
        Nothing -> assertFailure "Did not receive message answer"
        Just am -> messageAttributes am `shouldBe` testAttributes
    infoM "Pontarius.Xmpp" "Done running message mirror"
    infoM "Pontarius.Xmpp" "Running IQ tests"
    testPayload jid1 sess2
    infoM "Pontarius.Xmpp" "Done running IQ tests"
