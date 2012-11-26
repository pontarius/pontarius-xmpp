{-

Copyright © 2010-2012 Jon Kristensen.

This file (EchoClient.hs) illustrates how to connect, authenticate, set a simple
presence, receive message stanzas, and echo them back to whoever is sending
them, using Pontarius. The contents of this file may be used freely, as if it is
in the public domain.

-}


{-# LANGUAGE OverloadedStrings #-}


module Main (main) where

import Control.Concurrent
import Control.Monad (forever)
import Control.Monad.IO.Class (liftIO)
import Data.Maybe (fromJust, isJust)

import Network
import Network.Xmpp
import Network.Xmpp.Concurrent
import Network.Xmpp.IM


-- Server and authentication details.

host = "localhost"
hostname = "species64739.dyndns.org"

port = PortNumber 5222
username = "echouser"
password = "pwd"
resource = Nothing


-- TODO: Incomplete code, needs documentation, etc.
main :: IO ()
main = do
    csession <- newSessionChans
    withConnection (simpleConnect host port hostname username password resource)
        (session csession)
    forkIO $ autoAccept csession
    sendPresence presenceOnline csession
    echo csession
    return ()

-- Pull message stanzas, verify that they originate from a `full' XMPP
-- address, and, if so, `echo' the message back.
echo :: CSession -> IO ()
echo csession = forever $ do
    result <- pullMessage csession
    case result of
        Right message ->
            if (isJust $ messageFrom message) &&
                   (isFull $ fromJust $ messageFrom message) then do
                -- TODO: May not set from.
                sendMessage (Message Nothing (messageTo message) (messageFrom message) Nothing (messageType message) (messagePayload message)) csession
                liftIO $ putStrLn "Message echoed!"
            else liftIO $ putStrLn "Message sender is not set or is bare!"
        Left exception -> liftIO $ putStrLn "Error: "

-- | Autoaccept any subscription offers (So people can see us online)
autoAccept :: CSession -> IO ()
autoAccept csession = forever $ do
  st <- waitForPresence isPresenceSubscribe csession
  sendPresence (presenceSubscribed (fromJust $ presenceFrom st)) csession