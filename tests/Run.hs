{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import           Control.Applicative
import           Control.Concurrent
import           Control.Monad
import qualified Data.Configurator as Conf
import qualified Data.Configurator.Types as Conf
import           Data.Maybe
import qualified Data.Text as Text
import           Network
import           Network.Xmpp
import           System.Directory
import           System.FilePath
import           System.Log.Logger
import           System.Timeout
import           Test.HUnit
import           Test.Hspec.Expectations

import Run.Payload

xmppConfig :: ConnectionDetails -> SessionConfiguration
xmppConfig det = def{sessionStreamConfiguration
                          = def{connectionDetails = det}
                    , onConnectionClosed = \sess _ -> do
                          _ <- reconnect' sess
                          _ <- sendPresence presenceOnline sess
                          return ()
                    }

-- | Load the configuration files
loadConfig :: IO Conf.Config
loadConfig = do
    appData <- getAppUserDataDirectory "pontarius-xmpp-tests"
    home <- getHomeDirectory
    Conf.load [ Conf.Optional $ appData </> "pontarius-xmpp-tests.conf"
              , Conf.Optional $ home </> ".pontarius-xmpp-tests.conf"
              ]

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

main :: IO ()
main = void $ do
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
                Just p -> UseHost srv (PortNumber $ fromIntegral p)
    loglevel <- Conf.lookup conf "loglevel" >>= \case
        (Nothing :: Maybe Text.Text) -> return ERROR
        Just "debug" -> return DEBUG
        Just "info" -> return INFO
        Just "notice" -> return NOTICE
        Just "warning" -> return WARNING
        Just "error" -> return ERROR
        Just "critical" -> return CRITICAL
        Just "alert" -> return ALERT
        Just "emergency" -> return EMERGENCY
        Just e -> error $ "Log level " ++ (Text.unpack e) ++ " unknown"
    updateGlobalLogger "Pontarius.Xmpp" $ setLevel loglevel
    Right sess1 <- session realm (simpleAuth uname1 pwd1)
                                ((xmppConfig conDetails))
    Right sess2 <- session realm (simpleAuth uname2 pwd2)
                                ((xmppConfig conDetails))
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
