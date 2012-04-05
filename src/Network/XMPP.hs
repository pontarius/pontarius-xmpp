{-# LANGUAGE NoMonomorphismRestriction, OverloadedStrings  #-}
module Network.XMPP
  ( module Network.XMPP.Bind
  , module Network.XMPP.Concurrent
  , module Network.XMPP.Monad
  , module Network.XMPP.SASL
  , module Network.XMPP.Session
  , module Network.XMPP.Stream
  , module Network.XMPP.TLS
  , module Network.XMPP.Types
  , connectXMPP
  , sessionConnect
  ) where

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
fromHandle handle hostname username rsrc password a =
  xmppFromHandle handle hostname username rsrc $ do
      xmppStartStream
      -- this will check whether the server supports tls
      -- on it's own
      xmppStartTLS exampleParams
      xmppSASL password
      xmppBind rsrc
      xmppSession
      _ <- runThreaded a
      return ()

connectXMPP  :: HostName -> Text -> Text -> Maybe Text
                -> Text -> XMPPThread a -> IO ((), XMPPState)
connectXMPP host hostname username rsrc passwd a = do
  con <- connectTo host (PortNumber 5222)
  hSetBuffering con NoBuffering
  fromHandle con hostname username rsrc passwd a

sessionConnect  :: HostName -> Text -> Text
                   -> Maybe Text -> XMPPThread a -> IO (a, XMPPState)
sessionConnect host hostname username rsrc a = do
  con <- connectTo host (PortNumber 5222)
  hSetBuffering con NoBuffering
  xmppFromHandle con hostname username rsrc $
    xmppStartStream >> runThreaded a
