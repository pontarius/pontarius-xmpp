{-

Copyright © 2010-2012 Jon Kristensen.

This file (EchoClient.hs) illustrates how to connect, authenticate,
set a simple presence, receive message stanzas, and echo them back to
whoever is sending them, using Pontarius. The contents of this file
may be used freely, as if it is in the public domain.

-}


module Examples.EchoClient () where

import Network.XMPP


-- Server and authentication details.

hostName = "nejla.com"
portNumber = 5222
userName = "test"
password = ""


-- Start an XMPP session with the default settings, open the streams
-- to the XMPP server, authenticate, send a simple presence, and start
-- the `echo' XMPP thread.

main :: IO ()

main = session default $ do
  liftIO $ putStrLn "Welcome to the Pontarius EchoClient example!"
  openStreamsResult <- openStreams "nejla.com"
  case openStreamsResult of
    Nothing -> do
      liftIO $ putStrLn "Streams opened, now authenticating!"
      authenticateResult <- authenticate userName password Nothing
      case authenticateResult of
        Right _ -> do -- Ignore XMPP address
          liftIO $ putStrLn "Authenticating, now sending presence!"
          sendPresence Nothing Nothing [] Nothing -- Simple presence
          liftIO $ putStrLn "Echoing..."
          fork echo
        Left error -> liftIO $ putStrLn "Error: " ++ $ show exception
    Just error -> liftIO $ putStrLn "Error: " ++ $ show exception


-- Pull message stanzas, verify that they originate from a `full' XMPP
-- address, and, if so, `echo' the message back.

echo :: XMPPThread () -- TODO: XMPP ()? XMPP?

echo = forever $ do
  result <- pullMessage	
  case result of
    Right message ->
      if messageFrom message /= Nothing && isFull $ messageFrom message
      then do
        sendMessage (messageFrom message) (messageType message) Nothing []
        liftIO $ putStrLn "Message echoed!"
      else liftIO $ putStrLn "Message sender is not set or is bare!"
    Left exception -> liftIO $ putStrLn "Error: " ++ $ show exception