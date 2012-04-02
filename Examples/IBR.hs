{-

Copyright © 2010-2012 Jon Kristensen.

This file (IBR.hs) illustrates how to connect and perform a simple
In-Band Registration request using Pontarius. The contents of this
file may be used freely, as if it is in the public domain.

-}


module Examples.IBR () where

import Network.XMPP

import Control.Monad.IO.Class (liftIO)


-- Server details.

hostName = "nejla.com"
portNumber = 5222


-- The main function initializes Pontarius and specifies the (XMPPT)
-- actions the be executed, hooking the client into the appropriate
-- events and tries to open the streams to the server.

main :: IO ()

main = create $ do
  hookStreamsOpenedEvent onStreamsOpened Nothing
  hookDisconnectedEvent onDisconnected Nothing
  openStreams hostName portNumber
    
    where

      -- When the streams has been opened, print a message and unhook
      -- ourselves from future "Streams Opened" events.

        onStreamsOpened Nothing = do
            liftIO $ putStrLn $ "The server streams has been successfully opened."
            -- sendIQRequest Nothing hostName (LangTag "en" []) Set elem cb
            return False

        -- When the opening of the streams fails, print the error and
        -- shut down the XMPP session.

        onStreamsOpened (Just e) = do
            liftIO $ putStrLn $ "Could not open the streams due to the following error: " ++ (show e)
            destroy
            return True

        -- When disconnected, print the reason and shut down the XMPP
        -- session.

        onDisconnected r = do
            liftIO $ putStrLn $ "Disconnected with the reason: " ++ (show r)
            destroy
            return True
