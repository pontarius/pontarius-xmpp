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
import           Network.XMPP.Pickle

import           System.Environment
import Text.XML.Stream.Elements

testUser1 :: JID
testUser1 = read "testuser1@species64739.dyndns.org/bot1"

testUser2 :: JID
testUser2 = read "testuser2@species64739.dyndns.org/bot2"

supervisor :: JID
supervisor = read "uart14@species64739.dyndns.org"


attXmpp :: STM a -> XMPPThread a
attXmpp = liftIO . atomically

testNS :: Text
testNS = "xmpp:library:test"

data Payload = Payload Int Bool Text deriving (Eq, Show)

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
  (free, chan) <- listenIQChan Get testNS
  unless free $ liftIO $ putStrLn "Channel was already taken"
                >> error "hanging up"
  forever $ do
    next@(iq,_) <- liftIO . atomically $ readTChan chan
    let Right payload = unpickleElem payloadP $ iqRequestPayload iq
    let answerPayload = invertPayload payload
    let answerBody = pickleElem payloadP answerPayload
    answerIQ next (Right $ Just answerBody)

autoAccept :: XMPPThread ()
autoAccept = forever $ do
  st <- waitForPresence isPresenceSubscribe
  sendPresence $ presenceSubscribed (fromJust $ presenceFrom st)

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
  let (we, them, active) = case number of
                             1 -> (testUser1, testUser2,True)
                             2 -> (testUser2, testUser1,False)
                             _ -> error "Need either 1 or 2"
  let debug' = liftIO . atomically .
               debug . (("Thread " ++ show number ++ ":") ++)
  xmppNewSession $ do
      debug' "running"
      withConnection $ do
        xmppConnect "localhost" "species64739.dyndns.org"
        xmppStartTLS exampleParams
        saslResponse <- xmppSASL (fromJust $ localpart we) "pwd"
        case saslResponse of
          Right _ -> return ()
          Left e -> error e
      xmppThreadedBind (resourcepart we)
      withConnection $ xmppSession
      debug' "session standing"
      sendPresence presenceOnline
      forkXMPP autoAccept
      forkXMPP iqResponder
      when active . void . forkXMPP $ do
        forM [1..10] $ \count -> do
            let message = Text.pack . show $ localpart we
            let payload = Payload count (even count) (Text.pack $ show count)
            let body = pickleElem payloadP payload
            Right answer <- sendIQ' (Just them) Get Nothing body
            let Right answerPayload = unpickleElem payloadP
                                  (fromJust $ iqResultPayload answer)
            expect debug' (invertPayload payload) answerPayload
            liftIO $ threadDelay 100000
        sendUser "All tests done"
      liftIO  . forever $ threadDelay 10000000
      return ()
  return ()


main = do
  out <- newTChanIO
  forkIO . forever $ atomically (readTChan out) >>= putStrLn
  let debugOut = writeTChan out
  forkIO $ runMain debugOut 1
  runMain debugOut 2

