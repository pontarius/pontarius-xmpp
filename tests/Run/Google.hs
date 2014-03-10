{-# LANGUAGE OverloadedStrings #-}

-- | Test connecting to google services
module Run.Google where


import qualified Data.Configurator as Conf
import           Network.Xmpp
import           Network.Xmpp.Lens
import           System.Exit
import           System.Log.Logger
import           Test.HUnit
import           Network.TLS

import           Run.Config

xmppConf = set tlsServerIdentificationL ("talk.google.com", "") $ def

connectGoogle = do
    conf <- loadConfig
    _ <- configuredLoglevel conf
    infoM "Pontarius.Xmpp" "Trying to connect to google server"
    let realm = "google.com"
    user <- Conf.require conf "google.user"
    password <- Conf.require conf "google.password"
    mbSess <- session realm (simpleAuth user password) xmppConf
    sess <- case mbSess of
        Left e -> do
            assertFailure $ "google session could not be initialized" ++ show e
            exitFailure
        Right r -> return r
    infoM "Pontarius.Xmpp" "Done trying to connect to google server"

-- connectGoogleSCM = do
--     conf <- loadConfig
--     _ <- configuredLoglevel conf
--     infoM "Pontarius.Xmpp" "Trying to connect to google server"
--     let realm = "gcm.googleapis.com"
--     user <- Conf.require conf "google.user"
--     password <- Conf.require conf "google.password"
--     mbSess <- session realm (simpleAuth user password) xmppConf
--     sess <- case mbSess of
--         Left e -> do
--             assertFailure $ "google session could not be initialized" ++ show e
--             exitFailure
--         Right r -> return r
--     infoM "Pontarius.Xmpp" "Done trying to connect to google server"
