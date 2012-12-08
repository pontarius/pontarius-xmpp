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
realm    = "host.com"
username = "echo"
password = "pwd"
resource = Just "bot"

-- | Automatically accept all subscription requests from other entities
autoAccept :: Context -> IO ()
autoAccept context = forever $ do
  st <- waitForPresence isPresenceSubscribe context
  let Just friend = presenceFrom st
  sendPresence (presenceSubscribed friend) context
  printf "Hello %s !" (show friend)

main :: IO ()
main = do
    con <- simpleConnect
               host
               port
               realm
               username
               password
               resource
    putStrLn "connected"
    sendPresence presenceOnline con
    _thread <- forkIO $ autoAccept con
    forever $ do -- echo all messages back to the user
        msg <- getMessage con
        let sender = show . fromJust $ messageFrom msg
        let contents = maybe "nothing" Text.unpack $ body msg
        printf "%s sayd \"%s\"\n" sender contents
        sendMessage (answerIM (bodies msg) [] msg) con
    return ()
