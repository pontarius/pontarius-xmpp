{-

This directory defines a project that illustrates how to connect, authenticate,
set a simple presence, receive message stanzas, and echo them back to whoever is
sending them, using Pontarius XMPP. This file is in the public domain.

-}

{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad
import Data.Default
import Network.Xmpp
import System.Log.Logger

main :: IO ()
main = do
    updateGlobalLogger "Pontarius.Xmpp" $ setLevel DEBUG
    result <- session
                 "example.com"
                  def
                  (Just ([scramSha1 "user" Nothing "Password"], Nothing))
    sess <- case result of
                Right (s, Nothing) -> return s
                Right (_s, e) -> error $ "AuthFailure: " ++ (show e)
                Left e -> error $ "XmppFailure: " ++ (show e)
    sendPresence (Presence Nothing Nothing Nothing Nothing Nothing []) sess
    forever $ do
        msg <- getMessage sess
        case answerMessage msg (messagePayload msg) of
            Just answer -> sendMessage answer sess
            Nothing -> putStrLn "Received message with no sender."
