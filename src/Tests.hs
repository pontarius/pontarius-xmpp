{-# LANGUAGE PackageImports, OverloadedStrings #-}
module Example where

import           Network.XMPP
import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Monad
import           Control.Monad.IO.Class

import           Data.Maybe
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.XML.Pickle
import           Data.XML.Types

import           Network.XMPP.Pickle

import           System.Environment

testUser1 :: JID
testUser1 = read "testuser1@species64739.dyndns.org/bot1"

testUser2 :: JID
testUser2 = read "testuser2@species64739.dyndns.org/bot2"

superviser :: JID
superviser = read "uart14@species64739.dyndns.org"


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
    let payload = unpickleElem payloadP $ iqBody iq
    let answerPayload = invertPayload payload
    let answerBody = pickleElem payloadP answerPayload
    answerIQ next answerBody

autoAccept :: XMPPThread ()
autoAccept = forever $ do
  st <- pullPresence
  case st of
    Presence from _ idq (Just Subscribe) _ _ _ _  ->
      sendS . SPresence $
           Presence Nothing from idq (Just Subscribed) Nothing Nothing Nothing []
    _ -> return ()

sendUser txt = sendS . SMessage $ Message Nothing superviser Nothing Nothing Nothing
        (Just (Text.pack txt)) Nothing []


expect debug x y | x == y = debug "Ok."
                 | otherwise = do
                            let failMSG = "failed" ++ show x ++ " /= " ++ show y
                            debug failMSG
                            sendUser failMSG



runMain :: (String -> STM ()) -> Int -> IO ()
runMain debug number = do
  let (we, them, active) = case number of
                             1 -> (testUser1, testUser2,True)
                             2 -> (testUser2, testUser1,False)
                             _ -> error "Need either 1 or 2"
  sessionConnect "localhost"
                 "species64739.dyndns.org"
                 (fromJust $ node we) (resource we) $ do
      let debug' = liftIO . atomically . debug .
                    (("Thread " ++ show number ++ ":") ++)
      singleThreaded $ xmppSASL "pwd"
      xmppThreadedBind (resource we)
      singleThreaded $ xmppSession
      sendS . SPresence $ Presence Nothing Nothing Nothing Nothing (Just Available) Nothing Nothing []
      forkXMPP autoAccept
      forkXMPP iqResponder
      -- sendS . SPresence $ Presence Nothing (Just them) Nothing (Just Subscribe) Nothing Nothing Nothing  []
      let delay = if active then 1000000 else 5000000
      when active . void . forkXMPP . void . forM [1..10] $ \count -> do
        let message = Text.pack . show $ node we
        let payload = Payload count (even count) (Text.pack $ show count)
        let body = pickleElem payloadP payload
        answer <- sendIQ' (Just them) Get body
        let answerPayload = unpickleElem payloadP (iqBody answer)
        expect debug' (invertPayload payload) answerPayload
        liftIO $ threadDelay delay
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

