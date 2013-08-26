{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE QuasiQuotes #-}
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
import           Network.Xmpp.IM.Presence
import           Network.Xmpp.Internal
import           Network.Xmpp.Marshal
import           Network.Xmpp.Types
import           Network.Xmpp.Utilities (renderElement)
-- import qualified Network.Xmpp.Xep.InbandRegistration as IBR
import           Data.Default (def)
import qualified Network.Xmpp.Xep.ServiceDiscovery as Disco
import           System.Environment
import           System.Log.Logger

testUser1 :: Jid
testUser1 = [jidQ|echo1@species64739.dyndns.org/bot|]

testUser2 :: Jid
testUser2 = [jidQ|echo2@species64739.dyndns.org/bot|]

supervisor :: Jid
supervisor = [jidQ|uart14@species64739.dyndns.org|]

config = def{sessionStreamConfiguration
              = def{connectionDetails = UseHost "localhost" (PortNumber 5222)}}

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
  chan' <- listenIQChan Set testNS context
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


autoAccept :: Xmpp ()
autoAccept context = forever $ do
    st <- waitForPresence (\p -> presenceType p == Subscribe) context
    sendPresence (presenceSubscribed (fromJust $ presenceFrom st)) context

showPresence context = forever $ do
    pr <- waitForPresence (const True) context
    print $ getIMPresence pr


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
    q <- Disco.queryInfo [jidQ|species64739.dyndns.org|] Nothing context
    case q of
        Left (Disco.DiscoXmlError el e) -> do
            debug (show $ renderElement el)
            debug (ppUnpickleError e)
            debug (show $ length $ elementNodes el)
        x -> debug $ show x

    q <-  Disco.queryItems [jidQ|species64739.dyndns.org|]
                     (Just "http://jabber.org/protocol/commands") context
    case q of
        Left (Disco.DiscoXmlError el e) -> do
            debug (show $ renderElement el)
            debug (ppUnpickleError e)
            debug (show $ length $ elementNodes el)
        x -> debug $ show x

iqTest debug we them context = do
    forM [1..10] $ \count -> do
        let message = Text.pack . show $ localpart we
        let payload = Payload count (even count) (Text.pack $ show count)
        let body = pickleElem payloadP payload
        debug "sending"
        answer <- sendIQ' (Just them) Set Nothing body context
        case answer of
            Nothing -> debug "Connection Down"
            Just (IQResponseResult r) -> do
                debug "received"
                let Right answerPayload = unpickleElem payloadP
                                      (fromJust $ iqResultPayload r)
                expect debug (invertPayload payload) answerPayload context
            Just IQResponseTimeout -> do
                debug $ "Timeout in packet: " ++ show count
            Just (IQResponseError e) -> do
                debug $ "Error in packet: " ++ show count
        liftIO $ threadDelay 100000
--    sendUser "All tests done" context
    debug "ending session"

-- ibrTest debug uname pw = IBR.registerWith [ (IBR.Username, "testuser2")
--                                  , (IBR.Password, "pwd")
--                                  ] >>= debug . show


runMain :: (String -> STM ()) -> Int -> Bool -> IO ()
runMain debug number multi = do
  let (we, them, active) = case number `mod` 2 of
                             1 -> (testUser1, testUser2,True)
                             0 -> (testUser2, testUser1,False)
  let debug' = liftIO . atomically .
               debug . (("Thread " ++ show number ++ ":") ++)
  debug' "running"
  Right context <- session (Text.unpack $ domainpart we)
               (Just (\_ -> [scramSha1 (fromJust $ localpart we) Nothing "pwd"], resourcepart we))
                config
  sendPresence presenceOnline context
  thread1 <- forkIO $ autoAccept =<< dupSession context
  thread2 <- forkIO $ iqResponder =<< dupSession context
  when active $ do
    liftIO $ threadDelay 1000000 -- Wait for the other thread to go online
    discoTest debug' context
--    when multi $ iqTest debug' we them context
    killThread thread1
    killThread thread2
  return ()
  liftIO . threadDelay $ 10^6
--  unless multi . void .  withConnection $ IBR.unregister
  liftIO . forever $ threadDelay 1000000
  return ()

run i multi = do
  out <- newTChanIO
  debugger <- forkIO . forever $ atomically (readTChan out) >>= putStrLn
  let debugOut = writeTChan out
  when multi . void $ forkIO $ runMain debugOut (1 + i) multi
  runMain debugOut (2 + i) multi


main = do
    updateGlobalLogger "Pontarius.Xmpp" $ setLevel DEBUG
    run 1 False


connectionClosedTest = do
  updateGlobalLogger "Pontarius.Xmpp" $ setLevel DEBUG
  let debug' = infoM "Pontarius.Xmpp"
  debug' "running"
  let we = testUser1
  Right context <- session (Text.unpack $ domainpart we)
               (Just (\_ -> [scramSha1 (fromJust $ localpart we) Nothing "pwd"], resourcepart we))
                config {onConnectionClosed = \s e -> do
                             liftIO $ reconnect' s
                             liftIO $ sendPresence presenceOnline s
                             return ()
                       }
  sendPresence presenceOnline context
  forkIO $ do
      threadDelay 5000000
      closeConnection context
      debug' "done"
  forever $ threadDelay 1000000
  return ()
