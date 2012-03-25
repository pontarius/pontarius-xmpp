{-

Copyright © 2010-2012 Jon Kristensen.

This file (EchoClient.hs) illustrates how to connect, authenticate,
set a presence, and echo messages using Pontarius. The contents of
this file may be used freely, as if it is in the public domain.

-}


module Examples.EchoClient () where

import Network.XMPP


-- Account and server details.

hostName = "nejla.com"
userName = "pontarius"
serverIdentifier = "nejla.com"
portNumber = 5222
resource = "pontarius"
password = ""


-- The main function initializes PontariusP and specifies the (XMPPT)
-- actions the be executed, hooking the client into the appropriate
-- events and tries to connect.

main :: IO ()

main = runXMPPT $ do
    hookConnectedEvent onConnectedEvent Nothing
    hookMessageEvent onMessageEvent onMessageEventPredicate
    hookDisconnectedEvent onDisonnectedEvent Nothing
    connect hostName portNumber userName serverIdentifier password (Just resource)

    where

        -- When successfully connected, send a simple presence, and
        -- unhook ourselves from further "connected" events.

        onConnectedEvent (Right r) = do
            liftIO $ putStrLn $ "Connected with resource: " ++ (show r)
            presence simplePresence
            return False

        -- When the connection fails, print the error and shut down
        -- the XMPP session.

        onConnectedEvent (Left e) = do
            liftIO $ putStrLn $ "Could not connect due to the following error:" ++ (show e)
            destroy
            return True

        -- Predicate that makes sure that the messages processed by
        -- onMessageEvent are sent from and to full (not bare) XMPP
        -- addresses.

        onMessageEventPredicate = Just (\ m -> return $ and [isJust $ messageFrom m, isJust $ messageTo m])

        -- Swap the from and to addresses and send the new message.

        onMessageEvent m = do
            message $ m { messageFrom = fromJust $ messageTo m
                        , messageTo = fromJust $ messageFrom m }
            return True

        -- When disconnected, print the reason and shut down the XMPP
        -- session.

        onDisconnectedEvent r = do
            liftIO $ putStrLn $ "Disconnected with the reason: " ++ (show r)
            destroy
            return True
