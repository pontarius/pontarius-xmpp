{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}


module Tests.Common where

import           Control.Concurrent.Async
import           Control.Exception as Ex
import           Data.Default
import           Data.Maybe (fromJust)
import qualified Data.Text as Text
import           Data.Typeable (Typeable)
import           Network
import           Network.Xmpp
import           Test.Hspec

timeoutDuration :: Num a => a
timeoutDuration = 3000000

initiator :: Jid
initiator = [jidQ|echo1@species64739.dyndns.org/bot|]

responder :: Jid
responder = [jidQ|echo2@species64739.dyndns.org/bot|]

data TestAssertionFailed = TestAssertionFailed String deriving (Show, Eq, Typeable)
instance Exception TestAssertionFailed

assertionFailed :: String -> IO a
assertionFailed = throwIO . TestAssertionFailed

config :: SessionConfiguration
config = def{sessionStreamConfiguration
              = def{connectionDetails = UseHost "localhost" (PortNumber 5222)}}

createSession :: Jid -> IO Session
createSession we = do
    context' <- session (Text.unpack $ domainpart we)
                (Just ( \_ -> [scramSha1 (fromJust $ localpart we) Nothing "pwd"]
                      , resourcepart we)) config
    sess <- case context' of
          Left _e -> assertionFailed  "Session could not be initialized"
          Right r -> return r
    _ <- sendPresence presenceOnline sess `shouldReturn` (Right ())
    return sess

prepareThreads :: (Session -> IO a) -> (Session -> IO c) -> IO c
prepareThreads resp ini = bracket (createSession responder)
                                   endSession
                           (\respSession -> withAsync (resp respSession) $
                                            \_ -> bracket (createSession initiator)
                                                          endSession
                                                          ini)


-- startUp = do
--     thread1 <- forkIO $
