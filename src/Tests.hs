{-# LANGUAGE PackageImports, OverloadedStrings, NoMonomorphismRestriction #-}
module Example where

import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Monad
import           Control.Monad.IO.Class

import           Data.Maybe
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.XML.Pickle
import           Data.XML.Types

import           Network.XMPP
import           Network.XMPP.IM.Presence
import           Network.XMPP.Pickle

import           System.Environment
import Text.XML.Stream.Elements

testUser1 :: JID
testUser1 = read "testuser1@species64739.dyndns.org/bot1"

testUser2 :: JID
testUser2 = read "testuser2@species64739.dyndns.org/bot2"

supervisor :: JID
supervisor = read "uart14@species64739.dyndns.org"


attXmpp :: STM a -> XMPP a
attXmpp = liftIO . atomically

testNS :: Text
testNS = "xmpp:library:test"

data Payload = Payload
               { payloadCounter ::Int
               , payloadFlag :: Bool
               , payloadText :: Text
               } deriving (Eq, Show)

payloadP = xpWrap (\((counter,flag) , message) -> Payload counter flag message)
                  (\(Payload counter flag message) ->((counter,flag) , message)) $
                   xpElem (Name "request" (Just testNS) Nothing)
                      (xpPair
                        (xpAttr "counter" xpPrim)
                        (xpAttr "flag" xpPrim)
                      )
                      (xpElemNodes (Name "message" (Just testNS) Nothing)
                          (xpContent xpId))

invertPayload (Payload count flag message) = Payload (count + 1) (not flag) (Text.reverse message)

iqResponder = do
  chan' <- listenIQChan Get testNS
  chan <- case chan' of
      Nothing -> liftIO $ putStrLn "Channel was already taken"
                     >> error "hanging up"
      Just c -> return c
  forever $ do
    next@(iq,_) <- liftIO . atomically $ readTChan chan
    let Right payload = unpickleElem payloadP $ iqRequestPayload iq
    let answerPayload = invertPayload payload
    let answerBody = pickleElem payloadP answerPayload
    answerIQ next (Right $ Just answerBody)
    when (payloadCounter payload == 10) $ do
        liftIO $ threadDelay 1000000
        endSession

autoAccept :: XMPP ()
autoAccept = forever $ do
  st <- waitForPresence isPresenceSubscribe
  sendPresence $ presenceSubscribed (fromJust $ presenceFrom st)

simpleMessage :: JID -> Text -> Message
simpleMessage to txt = message
    { messageTo = Just to
    , messagePayload = [Element "body"
                        []
                        [NodeContent $ ContentText txt]
                       ]
    }
  where
    message = Message { messageID      = Nothing
                      , messageFrom    = Nothing
                      , messageTo      = Nothing
                      , messageLangTag = Nothing
                      , messageType    = Normal
                      , messagePayload = []
                      }


sendUser  = sendMessage . simpleMessage supervisor . Text.pack

expect debug x y | x == y = debug "Ok."
                 | otherwise = do
                            let failMSG = "failed" ++ show x ++ " /= " ++ show y
                            debug failMSG
                            sendUser failMSG


wait3 :: MonadIO m => m ()
wait3 = liftIO $ threadDelay 1000000

runMain :: (String -> STM ()) -> Int -> IO ()
runMain debug number = do
  let (we, them, active) = case number `mod` 2 of
                             1 -> (testUser1, testUser2,True)
                             0 -> (testUser2, testUser1,False)
  let debug' = liftIO . atomically .
               debug . (("Thread " ++ show number ++ ":") ++)
  wait <- newEmptyTMVarIO
  withNewSession $ do
      setSessionEndHandler (liftIO . atomically $ putTMVar wait ())
      setConnectionClosedHandler (\e -> do
                  liftIO (debug' $ "connection lost because " ++ show e)
                  endSession )
      debug' "running"
      withConnection $ do
          connect "localhost" "species64739.dyndns.org"
          startTLS exampleParams
          saslResponse <- auth (fromJust $ localpart we) "pwd" (resourcepart we)
          case saslResponse of
              Right _ -> return ()
              Left e -> error $ show e
          debug' "session standing"
      sendPresence presenceOnline
      fork autoAccept
      sendPresence $ presenceSubscribe them
      fork iqResponder
      when active $ do
        liftIO $ threadDelay 1000000 -- Wait for the other thread to go online
        void . fork $ do
            forM [1..10] $ \count -> do
                let message = Text.pack . show $ localpart we
                let payload = Payload count (even count) (Text.pack $ show count)
                let body = pickleElem payloadP payload
                debug' "sending"
                Right answer <- sendIQ' (Just them) Get Nothing body
                debug' "received"
                let Right answerPayload = unpickleElem payloadP
                                      (fromJust $ iqResultPayload answer)
                expect debug' (invertPayload payload) answerPayload
                liftIO $ threadDelay 100000
            sendUser "All tests done"
            debug' "ending session"
            liftIO . atomically $ putTMVar wait ()
            endSession
      liftIO . atomically $ takeTMVar wait
      return ()
  return ()

run i = do
  out <- newTChanIO
  forkIO . forever $ atomically (readTChan out) >>= putStrLn
  let debugOut = writeTChan out
  forkIO $ runMain debugOut (1 + i)
  runMain debugOut (2 + i)

main = run 0

