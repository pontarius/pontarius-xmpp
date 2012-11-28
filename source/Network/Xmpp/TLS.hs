{-# OPTIONS_HADDOCK hide #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.Xmpp.TLS where

import qualified Control.Exception.Lifted as Ex
import           Control.Monad
import           Control.Monad.Error
import           Control.Monad.State.Strict

import           Data.Conduit.TLS as TLS
import           Data.Typeable
import           Data.XML.Types

import           Network.Xmpp.Monad
import           Network.Xmpp.Pickle(ppElement)
import           Network.Xmpp.Stream
import           Network.Xmpp.Types

starttlsE :: Element
starttlsE = Element "{urn:ietf:params:xml:ns:xmpp-tls}starttls" [] []

exampleParams :: TLS.TLSParams
exampleParams = TLS.defaultParamsClient
    { pConnectVersion    = TLS.TLS10
    , pAllowedVersions   = [TLS.SSL3, TLS.TLS10, TLS.TLS11]
    , pCiphers           = [TLS.cipher_AES128_SHA1]
    , pCompressions      = [TLS.nullCompression]
    , pUseSecureRenegotiation = False -- No renegotiation
    , onCertificatesRecv = \_certificate ->
          return TLS.CertificateUsageAccept
    }

-- | Error conditions that may arise during TLS negotiation.
data XmppTLSError = TLSError TLSError
                  | TLSNoServerSupport
                  | TLSNoConnection
                  | TLSStreamError StreamError
                  | XmppTLSError -- General instance used for the Error instance
                    deriving (Show, Eq, Typeable)

instance Error XmppTLSError where
  noMsg = XmppTLSError

-- Pushes "<starttls/>, waits for "<proceed/>", performs the TLS handshake, and
-- restarts the stream. May throw errors.
startTLS :: TLS.TLSParams -> XmppConMonad (Either XmppTLSError ())
startTLS params = Ex.handle (return . Left . TLSError) . runErrorT $ do
    features <- lift $ gets sFeatures
    handle' <- lift $ gets sConHandle
    handle <- maybe (throwError TLSNoConnection) return handle'
    when (stls features == Nothing) $ throwError TLSNoServerSupport
    lift $ pushElement starttlsE
    answer <- lift $ pullElement
    case answer of
        Element "{urn:ietf:params:xml:ns:xmpp-tls}proceed" [] [] -> return ()
        Element "{urn:ietf:params:xml:ns:xmpp-tls}failure" _ _ ->
            lift . Ex.throwIO $ StreamConnectionError
            -- TODO: find something more suitable
        e -> lift . Ex.throwIO . StreamXMLError $
            "Unexpected element: " ++ ppElement e
    (raw, _snk, psh, ctx) <- lift $ TLS.tlsinit debug params handle
    lift $ modify ( \x -> x
                  { sRawSrc = raw
--                , sConSrc =  -- Note: this momentarily leaves us in an
                               -- inconsistent state
                  , sConPushBS = catchPush . psh
                  , sCloseConnection = TLS.bye ctx >> sCloseConnection x
                  })
    either (lift . Ex.throwIO) return =<< lift xmppRestartStream
    modify (\s -> s{sConnectionState = XmppConnectionSecured})
    return ()
