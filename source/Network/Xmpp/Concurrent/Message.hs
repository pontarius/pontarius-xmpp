{-# OPTIONS_HADDOCK hide #-}
module Network.Xmpp.Concurrent.Message where

import Control.Applicative((<$>))
import Network.Xmpp.Concurrent.Types
import Control.Concurrent.STM
import Network.Xmpp.Types
import Network.Xmpp.Concurrent.Basic

-- | Read an element from the inbound stanza channel, discardes any
-- non-Message stanzas from the channel
pullMessage :: Session -> IO (Either (Annotated MessageError) (Annotated Message))
pullMessage session = do
    (stanza, as) <- atomically . readTChan $ stanzaCh session
    case stanza of
        MessageS m      -> return $ Right (m, as)
        MessageErrorS e -> return $ Left  (e, as)
        _ -> pullMessage session

-- | Get the next received message with plugin Annotations
getMessageA :: Session -> IO (Annotated Message)
getMessageA = waitForMessageA (const True)

-- | Get the next received message
getMessage :: Session -> IO Message
getMessage s = fst <$> getMessageA s

-- | Pulls a (non-error) message and returns it if the given predicate returns
-- @True@.
waitForMessageA :: (Annotated Message -> Bool) -> Session -> IO (Annotated Message)
waitForMessageA f session = do
    s <- pullMessage session
    case s of
        Left _ -> waitForMessageA f session
        Right m | f m -> return m
                | otherwise -> waitForMessageA f session

waitForMessage :: (Message -> Bool) -> Session -> IO Message
waitForMessage f s  = fst <$> waitForMessageA (f . fst) s

-- | Pulls an error message and returns it if the given predicate returns @True@.
waitForMessageErrorA :: (Annotated MessageError -> Bool)
                    -> Session
                    -> IO (Annotated MessageError)
waitForMessageErrorA f session = do
    s <- pullMessage session
    case s of
        Right _ -> waitForMessageErrorA f session
        Left  m | f m -> return m
                | otherwise -> waitForMessageErrorA f session

waitForMessageError :: (MessageError -> Bool) -> Session -> IO MessageError
waitForMessageError f s  = fst <$> waitForMessageErrorA (f . fst) s

-- | Pulls a message and returns it if the given predicate returns @True@.
filterMessagesA :: (Annotated MessageError -> Bool)
               -> (Annotated Message -> Bool)
               -> Session -> IO (Either (Annotated MessageError)
                                        (Annotated Message))
filterMessagesA f g session = do
    s <- pullMessage session
    case s of
        Left  e | f e -> return $ Left e
                | otherwise -> filterMessagesA f g session
        Right m | g m -> return $ Right m
                | otherwise -> filterMessagesA f g session

filterMessages :: (MessageError -> Bool)
               -> (Message -> Bool)
               -> Session
               -> IO (Either (Annotated MessageError) (Annotated Message))
filterMessages f g s = filterMessagesA (f . fst) (g . fst) s

-- | Send a message stanza. Returns @False@ when the 'Message' could not be
-- sent.
sendMessage :: Message -> Session -> IO (Either XmppFailure ())
sendMessage m session = sendStanza (MessageS m) session
