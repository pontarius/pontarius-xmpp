{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings  #-}

module Network.XMPP.TLS where

import           Control.Applicative((<$>))
import           Control.Arrow(left)
import qualified Control.Exception.Lifted as Ex
import           Control.Monad
import           Control.Monad.Error
import           Control.Monad.State.Strict
import           Control.Monad.Trans

import           Data.Conduit
import           Data.Conduit.List as CL
import           Data.Conduit.TLS as TLS
import           Data.Default
import           Data.Typeable
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

data XMPPTLSError = TLSError TLSError
                  | TLSNoServerSupport
                  | TLSNoConnection
                  | TLSStreamError StreamError
                    deriving (Show, Eq, Typeable)

instance Error XMPPTLSError where
  noMsg = TLSNoConnection -- TODO: What should we choose here?
instance Ex.Exception XMPPTLSError


xmppStartTLS :: TLS.TLSParams -> XMPPConMonad (Either XMPPTLSError ())
xmppStartTLS params = Ex.handle (return . Left . TLSError)
  . runErrorT $ do
      features <- lift $ gets sFeatures
      handle' <- lift $ gets sConHandle
      handle <- maybe (throwError TLSNoConnection) return handle'
      when (stls features == Nothing) $ throwError TLSNoServerSupport
      lift $ pushN starttlsE
      answer <- lift $ pullE
      case answer of
        Element "{urn:ietf:params:xml:ns:xmpp-tls}proceed" [] [] -> return ()
        _ -> throwError $ TLSStreamError StreamXMLError
      (raw, snk, psh) <- lift $ TLS.tlsinit params handle
      lift $ modify (\x -> x
                    { sRawSrc = raw
--                  , sConSrc =  -- Note: this momentarily leaves us in an
                                 -- inconsistent state
                    , sConPushBS = psh
                    })
      ErrorT $ (left TLSStreamError) <$> xmppRestartStream
      modify (\s -> s{sHaveTLS = True})
      return ()

