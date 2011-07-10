{-

Copyright © 2010-2011 Jon Kristensen.

This file (EchoClient.hs) illustrates how to connect, authenticate, set a
presence, and echo messages with Pontarius XMPP. The contents of this file may
be used freely, as if it is in the public domain.

In any state-aware function (function operating in the StateT monad) you can get
and set the current by writing

@CMS.get >>= \ state -> CMS.put $ state { stateTest = 10 } ...@

or, if you prefer the do-notation,

@do
  state <- CMS.get
  CMS.put $ state { stateTest = 10 }
  ...@

-}


{-# LANGUAGE MultiParamTypeClasses #-}


module Examples.EchoClient () where

import Network.XMPP

import qualified Control.Monad as CM
import qualified Control.Monad.State as CMS
import qualified Control.Monad.IO.Class as CMIC
import qualified Data.Maybe as DM


-- Account and server details.

hostName = "jonkristensen.com"
userName = "pontarius"
serverIdentifier = "jonkristensen.com"
portNumber = 5222
resource = "echo-client"
password = ""


-- The client state, containing the required Pontarius XMPP Session object. It
-- also contains a dummy integer value to illustrate how client states are used.

data State = State { stateSession :: Maybe (Session State IO)
                   , stateTest :: Integer }

defaultState :: State

defaultState = State { stateSession = Nothing
                     , stateTest = 5 }


instance ClientState State IO where
  putSession st se = st { stateSession = Just se }


-- This client defines one client handler, and only specifies the
-- messageReceived callback.

clientHandlers = [ClientHandler { messageReceived = Just messageReceived_
                                , presenceReceived = Nothing
                                , iqReceived = Nothing
                                , sessionTerminated = Nothing }]


-- The main function sets up the Pontarius XMPP session with the default client
-- state and client handler defined above, as well as specifying that the
-- sessionCreated function should be called when the session has been created.

main :: IO ()

main = do
  session
    defaultState
    clientHandlers
    sessionCreated


-- The session has been created. Let's try to open the XMPP stream!

sessionCreated :: CMS.StateT State IO ()

sessionCreated = do
  state <- CMS.get
  connect (DM.fromJust $ stateSession state) hostName portNumber
    (Just ("", \ x -> True)) (Just (userName, password, Just resource))
    connectCallback
  id <- getID (DM.fromJust $ stateSession state)
  CMIC.liftIO $ putStrLn $ "Unique ID acquired: " ++ id
  injectAction (DM.fromJust $ stateSession state) Nothing (do CMIC.liftIO $ putStrLn "Async action!"; return ())
  injectAction (DM.fromJust $ stateSession state) Nothing (do CMIC.liftIO $ putStrLn "Async action!"; return ())
  injectAction (DM.fromJust $ stateSession state) Nothing (do CMIC.liftIO $ putStrLn "Async action!"; return ())
  injectAction (DM.fromJust $ stateSession state) Nothing (do CMIC.liftIO $ putStrLn "Async action!"; return ())
  injectAction (DM.fromJust $ stateSession state) Nothing (do CMIC.liftIO $ putStrLn "Async action!"; return ())
  injectAction (DM.fromJust $ stateSession state) Nothing (do CMIC.liftIO $ putStrLn "Async action!"; return ())
  injectAction (DM.fromJust $ stateSession state) Nothing (do CMIC.liftIO $ putStrLn "Async action!"; return ())
  injectAction (DM.fromJust $ stateSession state) Nothing (do CMIC.liftIO $ putStrLn "Async action!"; return ())
  injectAction (DM.fromJust $ stateSession state) Nothing (do CMIC.liftIO $ putStrLn "Async action!"; return ())
  injectAction (DM.fromJust $ stateSession state) Nothing (do CMIC.liftIO $ putStrLn "Async action!"; return ())
  return ()


-- We have tried to connected, TLS secured and authenticated!

connectCallback :: ConnectResult -> CMS.StateT State IO ()

connectCallback r = do
  state <- CMS.get
  case r of
    ConnectSuccess _ _ _ -> do
      sendPresence (DM.fromJust $ stateSession state)
        Presence { presenceID = Nothing
                 , presenceFrom = Nothing
                 , presenceTo = Nothing
                 , presenceXMLLang  = Nothing
                 , presenceType = Available
                 , presencePayload = [] }
        Nothing Nothing Nothing
    _ -> do
      CMIC.liftIO $ putStrLn "Could not connect."
      return ()


-- A message (stanza) has been received. Let's echo it!

messageReceived_ :: Message -> CMS.StateT State IO Bool

messageReceived_ m = do
  state <- CMS.get
  CMIC.liftIO $ putStrLn $
    "Received a message; echoing it! By the way: Internal state is " ++
    (show $ stateTest state) ++ "."
  sendMessage (DM.fromJust $ stateSession state)
    Message { messageID = messageID m
            , messageFrom = Nothing
            , messageTo = messageFrom m
            , messageXMLLang = Nothing
            , messageType = messageType m
            , messagePayload = messagePayload m }
    Nothing (Just (0, (do CMIC.liftIO $ putStrLn "Timeout!"; return ()))) Nothing
  return True
