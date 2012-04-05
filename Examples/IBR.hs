{-

Copyright © 2010-2012 Jon Kristensen.

This file (IBR.hs) illustrates how to connect and perform an XEP-0077:
In-Band Registration registration using Pontarius. The contents of
this file may be used freely, as if it is in the public domain.

-}


module Examples.IBR () where

import Network.XMPP


-- Server and authentication details.

hostName = "nejla.com"
portNumber = 5222
userName = "test"
password = ""


-- Start an XMPP session with the default settings, open the streams
-- to the XMPP server, send the `register' IQ, wait for and interpret
-- the response, and destroy the session.

main :: IO ()

main = session default $ do
  liftIO $ putStrLn "Welcome to the Pontarius IBR example!"
  openStreamsResult <- openStreams "nejla.com"
  case openStreamsResult of
    Nothing -> do
      liftIO $ putStrLn "Streams opened, now registering!"
      pushIQReq Nothing Set query Nothing $ \reply -> do
        case reply of
          Right (IQResponse {}) -> liftIO $ putStrLn "Registered!" -- TODO: iqRequestPayload may be empty!
          Right (IQError {}) -> liftIO $ putStrLn "Registration error!" -- TODO: More details from error stanza
          Left _ -> liftIO $ putStrLn "Registration error!" -- TODO: More details from error
        destroy
    Just error -> liftIO $ putStrLn "Error: " ++ $ show exception
  where
    query :: Element
    query = undefined -- TODO: <query xmlns='jabber:iq:register'><username>userName</username><password>password</password></query>