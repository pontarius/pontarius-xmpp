{-# OPTIONS_HADDOCK hide #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.Xmpp.TLS where

import qualified Control.Exception.Lifted as Ex
import           Control.Monad
import           Control.Monad.Error
import           Control.Monad.State.Strict

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import           Data.Conduit
import qualified Data.Conduit.Binary as CB
import           Data.Conduit.TLS as TLS
import           Data.Typeable
import           Data.XML.Types

import           Network.Xmpp.Connection
import           Network.Xmpp.Pickle(ppElement)
import           Network.Xmpp.Stream
import           Network.Xmpp.Types

mkBackend con = Backend { backendSend = \bs -> void (cSend con bs)
                        , backendRecv = cRecv con
                        , backendFlush = cFlush con
                        , backendClose = cClose con
                        }
  where
    cutBytes n = do
        liftIO $ putStrLn "awaiting"
        mbs <- await
        liftIO $ putStrLn "done awaiting"
        case mbs of
            Nothing -> return BS.empty
            Just bs -> do
                let (a, b) = BS.splitAt n bs
                liftIO . putStrLn $
                    "remaining" ++ (show $ BS.length b) ++ " of " ++ (show n)

                unless (BS.null b) $ leftover b
                return a


cutBytes n = do
    liftIO $ putStrLn "awaiting"
    mbs <- await
    liftIO $ putStrLn "done awaiting"
    case mbs of
        Nothing -> return False
        Just bs -> do
            let (a, b) = BS.splitAt n bs
            liftIO . putStrLn $
                "remaining" ++ (show $ BS.length b) ++ " of " ++ (show n)

            unless (BS.null b) $ leftover b
            return True


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
                  | TLSConnectionSecured -- ^ Connection already secured
                  | TLSStreamError StreamError
                  | XmppTLSError -- General instance used for the Error instance
                    deriving (Show, Eq, Typeable)

instance Error XmppTLSError where
  noMsg = XmppTLSError

-- Pushes "<starttls/>, waits for "<proceed/>", performs the TLS handshake, and
-- restarts the stream. May throw errors.
startTLS :: TLS.TLSParams -> Connection -> IO (Either XmppTLSError ())
startTLS params con = Ex.handle (return . Left . TLSError)
                      . flip withConnection con
                      . runErrorT $ do
    features <- lift $ gets sFeatures
    state <- gets sConnectionState
    case state of
        XmppConnectionPlain -> return ()
        XmppConnectionClosed -> throwError TLSNoConnection
        XmppConnectionSecured -> throwError TLSConnectionSecured
    con <- lift $ gets cHand
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
    (raw, _snk, psh, read, ctx) <- lift $ TLS.tlsinit debug params (mkBackend con)
    let newHand = Hand { cSend = catchPush . psh
                       , cRecv = read
                       , cFlush = contextFlush ctx
                       , cClose = bye ctx >> cClose con
                       }
    lift $ modify ( \x -> x {cHand = newHand})
    either (lift . Ex.throwIO) return =<< lift restartStream
    modify (\s -> s{sConnectionState = XmppConnectionSecured})
    return ()
