{-# OPTIONS_HADDOCK hide #-}
module Network.Xmpp.Concurrent.Message where

import Network.Xmpp.Concurrent.Types
import Control.Concurrent.STM
import Network.Xmpp.Types
import Network.Xmpp.Concurrent.Basic

-- | Read an element from the inbound stanza channel, discardes any
-- non-Message stanzas from the channel
pullMessage :: Session -> IO (Either MessageError Message)
pullMessage session = do
    stanza <- atomically . readTChan $ stanzaCh session
    case stanza of
        MessageS m -> return $ Right m
        MessageErrorS e -> return $ Left e
        _ -> pullMessage session

-- | Get the next received message
getMessage :: Session -> IO Message
getMessage = waitForMessage (const True)

-- | Pulls a (non-error) message and returns it if the given predicate returns
-- @True@.
waitForMessage :: (Message -> Bool) -> Session -> IO Message
waitForMessage f session = do
    s <- pullMessage session
    case s of
        Left _ -> waitForMessage f session
        Right m | f m -> return m
                | otherwise -> waitForMessage f session

-- | Pulls an error message and returns it if the given predicate returns @True@.
waitForMessageError :: (MessageError -> Bool) -> Session -> IO MessageError
waitForMessageError f session = do
    s <- pullMessage session
    case s of
        Right _ -> waitForMessageError f session
        Left  m | f m -> return m
                | otherwise -> waitForMessageError f session


-- | Pulls a message and returns it if the given predicate returns @True@.
filterMessages :: (MessageError -> Bool)
               -> (Message -> Bool)
               -> Session -> IO (Either MessageError Message)
filterMessages f g session = do
    s <- pullMessage session
    case s of
        Left  e | f e -> return $ Left e
                | otherwise -> filterMessages f g session
        Right m | g m -> return $ Right m
                | otherwise -> filterMessages f g session

-- | Send a message stanza.
sendMessage :: Message -> Session -> IO ()
sendMessage m session = sendStanza (MessageS m) session
