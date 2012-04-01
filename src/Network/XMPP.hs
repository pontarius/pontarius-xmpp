{-# LANGUAGE NoMonomorphismRestriction, OverloadedStrings  #-}
module Network.XMPP where

import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.State

import qualified Data.ByteString as BS
import           Data.Text as Text

import           Network
import           Network.XMPP.Bind
import           Network.XMPP.Concurrent
import           Network.XMPP.Monad
import           Network.XMPP.SASL
import           Network.XMPP.Session
import           Network.XMPP.Stream
import           Network.XMPP.TLS
import           Network.XMPP.Types

import           System.IO

--fromHandle :: Handle -> Text -> Text -> Maybe Text -> Text -> IO ((), XMPPState)
fromHandle :: Handle -> Text -> Text -> Maybe Text -> Text -> XMPPThread a
            -> IO ((), XMPPState)
fromHandle handle hostname username resource password a =
  xmppFromHandle handle hostname username resource $ do
      xmppStartStream
      -- this will check whether the server supports tls
      -- on it's own
      xmppStartTLS exampleParams
      xmppSASL password
      xmppBind
      xmppSession
      runThreaded a
      return ()

connectXMPP  :: HostName -> Text -> Text -> Maybe Text
                -> Text -> XMPPThread a -> IO ((), XMPPState)
connectXMPP host hostname username resource passwd a = do
  con <- connectTo host (PortNumber 5222)
  hSetBuffering con NoBuffering
  fromHandle con hostname username resource passwd a
