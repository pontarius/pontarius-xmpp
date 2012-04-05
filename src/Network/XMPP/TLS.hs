{-# LANGUAGE OverloadedStrings  #-}

module Network.XMPP.TLS where

import           Control.Monad
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.State

import           Data.Conduit
import           Data.Conduit.List as CL
import           Data.Conduit.TLS as TLS
import           Data.Default
import           Data.XML.Types

import           Network.XMPP.Monad
import           Network.XMPP.Stream
import           Network.XMPP.Types

import qualified Text.XML.Stream.Render as XR


starttlsE :: Element
starttlsE =
  Element "{urn:ietf:params:xml:ns:xmpp-tls}starttls" [] []


exampleParams :: TLSParams
exampleParams = TLS.defaultParams {TLS.pCiphers = TLS.ciphersuite_strong}

xmppStartTLS :: TLSParams -> XMPPMonad ()
xmppStartTLS params = do
  features <- gets sFeatures
  unless (stls features == Nothing) $ do
      pushN starttlsE
      Element "{urn:ietf:params:xml:ns:xmpp-tls}proceed" [] [] <- pullE
      Just handle <- gets sConHandle
      (raw, snk, psh) <- lift $ TLS.tlsinit params handle
      modify (\x -> x
                     { sRawSrc = raw
--                   , sConSrc =  -- Note: this momentarily leaves us in an
                                  -- inconsistent state
                     , sConPush = \xs -> CL.sourceList xs
                     $$ XR.renderBytes def =$ snk
                     , sConPushBS = psh
                     })
      xmppRestartStream
      modify (\s -> s{sHaveTLS = True})
  return ()

