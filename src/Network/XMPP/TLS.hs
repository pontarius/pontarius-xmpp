{-# LANGUAGE OverloadedStrings  #-}

module Network.XMPP.TLS where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.State

import Data.Text(Text)

import Network.XMPP.Monad
import Network.XMPP.Stream
import Network.XMPP.Types

import Data.Conduit
import Data.Conduit.Hexpat as HX
import Data.Conduit.Text as CT
import Data.Conduit.TLS as TLS
import Data.Conduit.List as CL
import qualified Data.List as L

import Text.XML.Expat.Tree

starttlsE :: Node Text Text
starttlsE =
  Element "starttls" [("xmlns", "urn:ietf:params:xml:ns:xmpp-tls")] []


exampleParams :: TLSParams
exampleParams = TLS.defaultParams {TLS.pCiphers = TLS.ciphersuite_strong}

xmppStartTLS :: TLSParams -> XMPPMonad Bool
xmppStartTLS params = do
  features <- gets sFeatures
  unless (stls features == Nothing) $ do
      pushN starttlsE
      Element "proceed" [("xmlns", "urn:ietf:params:xml:ns:xmpp-tls")] [] <- pullE
      Just handle <- gets sConHandle
      (raw', snk) <- lift $ TLS.tlsinit params handle
      raw <- lift . bufferSource $ raw'
      modify (\x -> x
                     { sRawSrc = raw
--                   , sConSrc =  -- Note: this momentarily leaves us in an
                                  -- inconsistent state
                     , sConPush = liftIO . snk
                     })
      xmppRestartStream
      modify (\s -> s{sHaveTLS = True})
  gets sHaveTLS

