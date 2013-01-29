{-

Copyright © 2010-2012 Jon Kristensen, Philipp Balzarek

This file (EchoClient.hs) illustrates how to connect, authenticate, set a simple
presence, receive message stanzas, and echo them back to whoever is sending
them, using Pontarius. The contents of this file may be used freely, as if it is
in the public domain.

-}


{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Concurrent
import           Control.Monad
import           Data.Maybe (fromJust)
import qualified Data.Text as Text
import           Text.Printf

import           Network.Xmpp
import           Network.Xmpp.IM

-- Server and authentication details.
host     = "localhost"
port     = PortNumber 5222
realm    = "species64739.dyndns.org"
username = "echo"
password = "pwd"
resource = Just "bot"

-- | Automatically accept all subscription requests from other entities
autoAccept :: Session -> IO ()
autoAccept session = forever $ do
  st <- waitForPresence isPresenceSubscribe session
  let Just friend = presenceFrom st
  sendPresence (presenceSubscribed friend) session
  printf "Hello %s !" (show friend)

main :: IO ()
main = do
    sess <- simpleConnect
                host
                port
                realm
                username
                password
                resource
    -- We won't be able to receive stanzas before we set out status to online
    sendPresence presenceOnline sess
    putStrLn "Connected."
    -- We want to see all incoming stanzas in the auto-accept thread as well.
    sess' <- dupSession sess
    _thread <- forkIO $ autoAccept sess'
    forever $ do
        -- Echo all messages back to the user.
        msg <- getMessage sess
        sendMessage (answerIM (bodies msg) [] msg) sess
        -- Print the received message to the screen.
        let sender = show . fromJust $ messageFrom msg
        let contents = maybe "nothing" Text.unpack $ body msg
        printf "%s says \"%s\"\n" sender contents
    return ()
