{-

Copyright © 2010-2012 Jon Kristensen.

This file (EchoClient.hs) illustrates how to connect, authenticate, set a simple
presence, receive message stanzas, and echo them back to whoever is sending
them, using Pontarius. The contents of this file may be used freely, as if it is
in the public domain.

-}


{-# LANGUAGE OverloadedStrings #-}


module Main (main) where

import Control.Monad (forever)
import Control.Monad.IO.Class (liftIO)
import Data.Maybe (fromJust, isJust)

import Network.Xmpp
import Network.Xmpp.IM


-- Server and authentication details.

hostname = "nejla.com"
hostname_ = "xmpp.nejla.com" -- TODO
-- portNumber = 5222 -- TODO
userName = ""
password = ""


-- TODO: Incomplete code, needs documentation, etc.
main :: IO ()
main = do
    withNewSession $ do
        withConnection $ do
            connect hostname_ hostname
            -- startTLS exampleParams
            saslResponse <- simpleAuth userName password (Just "echo-client")
            case saslResponse of
                Right _ -> return ()
                Left e -> error $ show e
        sendPresence presenceOnline
        fork echo
        return ()
    return ()

-- Pull message stanzas, verify that they originate from a `full' Xmpp
-- address, and, if so, `echo' the message back.
echo :: Xmpp ()
echo = forever $ do
    result <- pullMessage
    case result of
        Right message ->
            if (isJust $ messageFrom message) &&
                   (isFull $ fromJust $ messageFrom message) then do
                -- TODO: May not set from.
                sendMessage $ Message Nothing (messageTo message) (messageFrom message) Nothing (messageType message) (messagePayload message)
                liftIO $ putStrLn "Message echoed!"
            else liftIO $ putStrLn "Message sender is not set or is bare!"
        Left exception -> liftIO $ putStrLn "Error: "