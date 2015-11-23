{-

This directory defines a project that illustrates how to connect, authenticate,
set a simple presence, receive message stanzas, and echo them back to whoever is
sending them, using Pontarius XMPP. This file is in the public domain.

-}

{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad
import Data.Default
import Lens.Family2
import Network.Xmpp
import Network.Xmpp.Internal (TlsBehaviour(..))
import System.Log.Logger


main :: IO ()
main = do
    updateGlobalLogger "Pontarius.Xmpp" $ setLevel DEBUG
    result <- session
                 "test.pontarius.org"
                  (Just (\_ -> ( [scramSha1 "testuser1" Nothing "pwd1"])
                               , Nothing))
                  $ def & streamConfigurationL . tlsBehaviourL .~ PreferPlain
                        & streamConfigurationL . connectionDetailsL .~
                            UseHost "localhost" 5222
                        & onConnectionClosedL .~ reconnectSession

    sess <- case result of
                Right s -> return s
                Left e -> error $ "XmppFailure: " ++ (show e)
    sendPresence def sess
    forever $ do
        msg <- getMessage sess
        case answerMessage msg (messagePayload msg) of
            Just answer -> sendMessage answer sess >> return ()
            Nothing -> putStrLn "Received message with no sender."
  where
    reconnectSession sess failure = reconnect' sess >> return ()
