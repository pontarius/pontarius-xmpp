{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings  #-}

module Network.XMPP.TLS where

import qualified Control.Exception.Lifted as Ex
import           Control.Monad
import           Control.Monad.Error
import           Control.Monad.State.Strict

import           Data.Conduit.TLS as TLS
import           Data.Typeable
import           Data.XML.Types

import           Network.XMPP.Monad
import           Network.XMPP.Pickle(ppElement)
import           Network.XMPP.Stream
import           Network.XMPP.Types

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
                      , onCertificatesRecv = \ _certificate ->
                                             return TLS.CertificateUsageAccept
                      }

-- | Error conditions that may arise during TLS negotiation.
data XMPPTLSError = TLSError TLSError
                  | TLSNoServerSupport
                  | TLSNoConnection
                  | TLSStreamError StreamError
                    deriving (Show, Eq, Typeable)

instance Error XMPPTLSError where
  noMsg = TLSNoConnection -- TODO: What should we choose here?

startTLS :: TLS.TLSParams -> XMPPConMonad (Either XMPPTLSError ())
startTLS params = Ex.handle (return . Left . TLSError)
  . runErrorT $ do
      features <- lift $ gets sFeatures
      handle' <- lift $ gets sConHandle
      handle <- maybe (throwError TLSNoConnection) return handle'
      when (stls features == Nothing) $ throwError TLSNoServerSupport
      lift $ pushN starttlsE
      answer <- lift $ pullElement
      case answer of
        Element "{urn:ietf:params:xml:ns:xmpp-tls}proceed" [] [] -> return ()
        Element "{urn:ietf:params:xml:ns:xmpp-tls}failure" _ _
            -> lift . Ex.throwIO $ StreamConnectionError
                 -- TODO: find something more suitable
        e -> lift . Ex.throwIO . StreamXMLError
               $ "Unexpected element: " ++ ppElement e
      (raw, _snk, psh, ctx) <- lift $ TLS.tlsinit params handle
      lift $ modify (\x -> x
                    { sRawSrc = raw
--                  , sConSrc =  -- Note: this momentarily leaves us in an
                                 -- inconsistent state
                    , sConPushBS = psh
                    , sCloseConnection = TLS.bye ctx >> sCloseConnection x
                    })
      either (lift . Ex.throwIO) return =<< lift xmppRestartStream
      modify (\s -> s{sHaveTLS = True})
      return ()

