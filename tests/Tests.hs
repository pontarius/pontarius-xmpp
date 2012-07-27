{-# LANGUAGE PackageImports, OverloadedStrings, NoMonomorphismRestriction #-}
module Example where

import           Control.Concurrent
import           Control.Concurrent.STM
import qualified Control.Exception.Lifted as Ex
import           Control.Monad
import           Control.Monad.IO.Class

import           Data.Maybe
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.XML.Pickle
import           Data.XML.Types

import           Network.Xmpp
import           Network.Xmpp.IM.Presence
import           Network.Xmpp.Pickle
import qualified Network.Xmpp.Xep.ServiceDiscovery as Disco
import qualified Network.Xmpp.Xep.InbandRegistration as IBR

import           System.Environment
import           Text.XML.Stream.Elements

testUser1 :: Jid
testUser1 = read "testuser1@species64739.dyndns.org/bot1"

testUser2 :: Jid
testUser2 = read "testuser2@species64739.dyndns.org/bot2"

supervisor :: Jid
supervisor = read "uart14@species64739.dyndns.org"


attXmpp :: STM a -> Xmpp a
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
      Left _ -> liftIO $ putStrLn "Channel was already taken"
                     >> error "hanging up"
      Right c -> return c
  forever $ do
    next <- liftIO . atomically $ readTChan chan
    let Right payload = unpickleElem payloadP . iqRequestPayload $
                          iqRequestBody next
    let answerPayload = invertPayload payload
    let answerBody = pickleElem payloadP answerPayload
    unless (payloadCounter payload == 3) . void $
        answerIQ next (Right $ Just answerBody)
    when (payloadCounter payload == 10) $ do
        liftIO $ threadDelay 1000000
        endSession

autoAccept :: Xmpp ()
autoAccept = forever $ do
  st <- waitForPresence isPresenceSubscribe
  sendPresence $ presenceSubscribed (fromJust $ presenceFrom st)

simpleMessage :: Jid -> Text -> Message
simpleMessage to txt = message
    { messageTo = Just to
    , messagePayload = [ Element
                             "body"
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

discoTest debug = do
    q <- Disco.queryInfo "species64739.dyndns.org" Nothing
    case q of
        Left (Disco.DiscoXMLError el e) -> do
            debug (ppElement el)
            debug (Text.unpack $ ppUnpickleError e)
            debug (show $ length $ elementNodes el)
        x -> debug $ show x

    q <-  Disco.queryItems "species64739.dyndns.org"
                     (Just "http://jabber.org/protocol/commands")
    case q of
        Left (Disco.DiscoXMLError el e) -> do
            debug (ppElement el)
            debug (Text.unpack $ ppUnpickleError e)
            debug (show $ length $ elementNodes el)
        x -> debug $ show x

iqTest debug we them = do
    forM [1..10] $ \count -> do
        let message = Text.pack . show $ localpart we
        let payload = Payload count (even count) (Text.pack $ show count)
        let body = pickleElem payloadP payload
        debug "sending"
        answer <- sendIQ' (Just them) Get Nothing body
        case answer of
            IQResponseResult r -> do
                debug "received"
                let Right answerPayload = unpickleElem payloadP
                                      (fromJust $ iqResultPayload r)
                expect debug (invertPayload payload) answerPayload
            IQResponseTimeout -> do
                debug $ "Timeout in packet: " ++ show count
            IQResponseError e -> do
                debug $ "Error in packet: " ++ show count
        liftIO $ threadDelay 100000
    sendUser "All tests done"
    debug "ending session"

ibrTest debug = IBR.requestFields >>= debug . show


runMain :: (String -> STM ()) -> Int -> Bool -> IO ()
runMain debug number multi = do
  let (we, them, active) = case number `mod` 2 of
                             1 -> (testUser1, testUser2,True)
                             0 -> (testUser2, testUser1,False)
  let debug' = liftIO . atomically .
               debug . (("Thread " ++ show number ++ ":") ++)
  withNewSession $ do
      setConnectionClosedHandler (\e -> do
                  liftIO (debug' $ "connection lost because " ++ show e)
                  endSession )
      debug' "running"
      withConnection $ Ex.catch (do
          connect "localhost" "species64739.dyndns.org"
          startTLS exampleParams
          ibrTest debug'
          saslResponse <- simpleAuth
                            (fromJust $ localpart we) "pwd" (resourcepart we)
          case saslResponse of
              Right _ -> return ()
              Left e -> error $ show e
          debug' "session standing")
          (\e -> debug' $ show  (e ::Ex.SomeException))
      sendPresence presenceOnline
      thread1 <- fork autoAccept
      sendPresence $ presenceSubscribe them
      thread2 <- fork iqResponder
      when active $ do
        liftIO $ threadDelay 1000000 -- Wait for the other thread to go online
        discoTest debug'
        when multi $ iqTest debug' we them
        closeConnection
        liftIO $ killThread thread1
        liftIO $ killThread thread2
      return ()
  return ()

run i multi = do
  out <- newTChanIO
  debugger <- forkIO . forever $ atomically (readTChan out) >>= putStrLn
  let debugOut = writeTChan out
  when multi . void $ forkIO $ runMain debugOut (1 + i) multi
  runMain debugOut (2 + i) multi


main = run 0 False
