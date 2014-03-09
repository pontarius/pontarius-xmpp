{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Run.Config where

import qualified Data.Configurator as Conf
import qualified Data.Configurator.Types as Conf
import           System.Directory
import           System.FilePath
import           System.Log.Logger
import qualified Data.Text as Text

-- | Load the configuration files
loadConfig :: IO Conf.Config
loadConfig = do
    appData <- getAppUserDataDirectory "pontarius-xmpp-tests"
    home <- getHomeDirectory
    Conf.load [ Conf.Optional $ appData </> "pontarius-xmpp-tests.conf"
              , Conf.Optional $ home </> ".pontarius-xmpp-tests.conf"
              ]

configuredLoglevel conf = do
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
    return loglevel
