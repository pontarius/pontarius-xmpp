{-# LANGUAGE NoMonomorphismRestriction, OverloadedStrings  #-}
module Network.XMPPConduit where

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.State

import qualified Data.ByteString as BS
import Data.Text as Text

import Network
import Network.XMPP.Monad
import Network.XMPP.TLS
import Network.XMPP.Stream
import Network.XMPP.SASL
import Network.XMPP.Types
import Network.XMPP.Bind


import System.IO

fromHandle :: Handle -> Text -> Text -> Text -> Maybe Text -> IO ((), XMPPState)
fromHandle handle hostname username password resource =
  xmppFromHandle handle hostname username resource $ do
      xmppStartStream
      -- this will check whether the server supports tls
      -- on it's own
      xmppStartTLS exampleParams
      xmppSASL password
      xmppBind
      gets sResource >>= liftIO . print
      gets sHaveTLS >>= liftIO . print
      forever $ pullE >>= liftIO . print
      return ()

main = do
  con <- connectTo "localhost" (PortNumber 5222)
  hSetBuffering con NoBuffering
  (fs,st) <- fromHandle con "species64739.dyndns.org" "bot" "pwd" (Just "botr")
  print $ sHaveTLS st
  putStrLn ""
  hGetContents con >>= putStrLn

