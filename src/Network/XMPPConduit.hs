{-# LANGUAGE NoMonomorphismRestriction, OverloadedStrings  #-}
module Network.XMPPConduit where

import Control.Monad
import Control.Monad.Trans

import qualified Data.ByteString as BS
import Data.Text as Text

import Network
import Network.XMPP.Monad
import Network.XMPP.TLS
import Network.XMPP.Stream
import Network.XMPP.SASL


import System.IO

fromHandle :: Handle -> Text -> Text -> Text ->  IO ((), XMPPState)
fromHandle handle hostname username password =
  xmppFromHandle handle hostname username "" $ do
      xmppStartStream
      -- this will check whether the server supports tls
      -- on it's own
      xmppStartTLS exampleParams
      xmppSASL password
      forever $ pull >>= liftIO . print
      return ()

main = do
  con <- connectTo "localhost" (PortNumber 5222)
  hSetBuffering con NoBuffering
  (fs,st) <- fromHandle con "species64739.dyndns.org" "bot" "pwd"
  print $ haveTLS st
  putStrLn ""
  hGetContents con >>= putStrLn

