{-# LANGUAGE PackageImports, OverloadedStrings, NoMonomorphismRestriction #-}
module Example where

import           Control.Concurrent
import           Control.Concurrent.STM
import qualified Control.Exception.Lifted as Ex
import           Control.Monad
import           Control.Monad.State
import           Control.Monad.IO.Class
import           Control.Monad.Reader

import           Data.Maybe
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.XML.Pickle
import           Data.XML.Types

import           Network
import           Network.Xmpp
import           Network.Xmpp.Concurrent.Channels
import           Network.Xmpp.IM.Presence
import           Network.Xmpp.Pickle
import           Network.Xmpp.Types
import qualified Network.Xmpp.Xep.InbandRegistration as IBR
import qualified Network.Xmpp.Xep.ServiceDiscovery as Disco

import           System.Environment
import           Text.XML.Stream.Elements

testUser1 :: Jid
testUser1 = read "testuser1@species64739.dyndns.org/bot1"

testUser2 :: Jid
testUser2 = read "testuser2@species64739.dyndns.org/bot2"

supervisor :: Jid
supervisor = read "uart14@species64739.dyndns.org"

testNS :: Text
testNS = "xmpp:library:test"

type Xmpp a = Session -> IO a

data Payload = Payload
               { payloadCounter :: Int
               , payloadFlag    :: Bool
               , payloadText    :: Text
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

iqResponder context = do
  chan' <- listenIQChan Get testNS context
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
        answerIQ next (Right $ Just answerBody) context
    when (payloadCounter payload == 10) $ do
        threadDelay 1000000
        endContext (session context)

autoAccept :: Xmpp ()
autoAccept context = forever $ do
  st <- waitForPresence isPresenceSubscribe context
  sendPresence (presenceSubscribed (fromJust $ presenceFrom st)) context

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

sendUser m context = sendMessage (simpleMessage supervisor $ Text.pack m) context

expect debug x y context | x == y = debug "Ok."
                          | otherwise = do
                              let failMSG = "failed" ++ show x ++ " /= " ++ show y
                              debug failMSG
                              sendUser failMSG context

wait3 :: MonadIO m => m ()
wait3 = liftIO $ threadDelay 1000000

discoTest debug context = do
    q <- Disco.queryInfo "species64739.dyndns.org" Nothing context
    case q of
        Left (Disco.DiscoXMLError el e) -> do
            debug (ppElement el)
            debug (Text.unpack $ ppUnpickleError e)
            debug (show $ length $ elementNodes el)
        x -> debug $ show x

    q <-  Disco.queryItems "species64739.dyndns.org"
                     (Just "http://jabber.org/protocol/commands") context
    case q of
        Left (Disco.DiscoXMLError el e) -> do
            debug (ppElement el)
            debug (Text.unpack $ ppUnpickleError e)
            debug (show $ length $ elementNodes el)
        x -> debug $ show x

iqTest debug we them context = do
    forM [1..10] $ \count -> do
        let message = Text.pack . show $ localpart we
        let payload = Payload count (even count) (Text.pack $ show count)
        let body = pickleElem payloadP payload
        debug "sending"
        answer <- sendIQ' (Just them) Get Nothing body context
        case answer of
            IQResponseResult r -> do
                debug "received"
                let Right answerPayload = unpickleElem payloadP
                                      (fromJust $ iqResultPayload r)
                expect debug (invertPayload payload) answerPayload context
            IQResponseTimeout -> do
                debug $ "Timeout in packet: " ++ show count
            IQResponseError e -> do
                debug $ "Error in packet: " ++ show count
        liftIO $ threadDelay 100000
    sendUser "All tests done" context
    debug "ending session"

fork action context = do
    context' <- forkSession context
    forkIO $ action context'

ibrTest debug uname pw = IBR.registerWith [ (IBR.Username, "testuser2")
                                 , (IBR.Password, "pwd")
                                 ] >>= debug . show


runMain :: (String -> STM ()) -> Int -> Bool -> IO ()
runMain debug number multi = do
  let (we, them, active) = case number `mod` 2 of
                             1 -> (testUser1, testUser2,True)
                             0 -> (testUser2, testUser1,False)
  let debug' = liftIO . atomically .
               debug . (("Thread " ++ show number ++ ":") ++)
  context <- newSession

  setConnectionClosedHandler (\e s -> do
              debug' $ "connection lost because " ++ show e
              endContext s) (session context)
  debug' "running"
  flip withConnection (session context) $ Ex.catch (do
      debug' "connect"
      connect "localhost" (PortNumber 5222) "species64739.dyndns.org"
--      debug' "tls start"
      startTLS exampleParams
      debug' "ibr start"
      -- ibrTest debug' (localpart we) "pwd"
      -- debug' "ibr end"
      saslResponse <- simpleAuth
                        (fromJust $ localpart we) "pwd" (resourcepart we)
      case saslResponse of
          Right _ -> return ()
          Left e -> error $ show e
      debug' "session standing"
      features <- other `liftM` gets sFeatures
      liftIO . void $ forM features $ \f -> debug' $ ppElement f
      )
      (\e -> debug' $ show  (e ::Ex.SomeException))
  sendPresence presenceOnline context
  thread1 <- fork autoAccept context
  sendPresence (presenceSubscribe them) context
  thread2 <- fork iqResponder context
  when active $ do
    liftIO $ threadDelay 1000000 -- Wait for the other thread to go online
--    discoTest debug'
    when multi $ iqTest debug' we them context
    closeConnection (session context)
    killThread thread1
    killThread thread2
  return ()
  liftIO . threadDelay $ 10^6
--  unless multi . void .  withConnection $ IBR.unregister
  unless multi . void $ fork (\s -> forever $ do
                                                 pullMessage s >>= debug' . show
                                                 putStrLn ""
                                                 putStrLn ""
                             )
                             context
  liftIO . forever $ threadDelay 1000000
  return ()

run i multi = do
  out <- newTChanIO
  debugger <- forkIO . forever $ atomically (readTChan out) >>= putStrLn
  let debugOut = writeTChan out
  when multi . void $ forkIO $ runMain debugOut (1 + i) multi
  runMain debugOut (2 + i) multi


main = run 0 True
