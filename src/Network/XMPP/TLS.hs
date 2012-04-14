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

import qualified Network.TLS as TLS
import qualified Network.TLS.Extra as TLS
import           Network.XMPP.Monad
import           Network.XMPP.Stream
import           Network.XMPP.Types

import qualified Text.XML.Stream.Render as XR


starttlsE :: Element
starttlsE =
  Element "{urn:ietf:params:xml:ns:xmpp-tls}starttls" [] []

exampleParams :: TLS.TLSParams
exampleParams = TLS.defaultParams
                      {pConnectVersion    = TLS.TLS10
                      , pAllowedVersions   = [TLS.SSL3, TLS.TLS10, TLS.TLS11]
                      , pCiphers           = [TLS.cipher_AES128_SHA1]
                      , pCompressions      = [TLS.nullCompression]
                      , pWantClientCert    = False -- Used for servers
                      , pUseSecureRenegotiation = False -- No renegotiation
                      , pCertificates      = [] -- TODO
                      , pLogging           = TLS.defaultLogging -- TODO
                      , onCertificatesRecv = \ certificate ->
                                             return TLS.CertificateUsageAccept
                      }

xmppStartTLS :: TLS.TLSParams -> XMPPConMonad ()
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
                     , sConPushBS = psh
                     })
      xmppRestartStream
      modify (\s -> s{sHaveTLS = True})
  return ()

