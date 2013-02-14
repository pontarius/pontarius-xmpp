{-# OPTIONS_HADDOCK hide #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.Xmpp.Tls where

import qualified Control.Exception.Lifted as Ex
import           Control.Monad
import           Control.Monad.Error
import           Control.Monad.State.Strict

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import           Data.Conduit
import qualified Data.Conduit.Binary as CB
import           Data.Conduit.Tls as TLS
import           Data.Typeable
import           Data.XML.Types

import           Network.Xmpp.Stream
import           Network.Xmpp.Types

import           Control.Concurrent.STM.TMVar

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

-- Pushes "<starttls/>, waits for "<proceed/>", performs the TLS handshake, and
-- restarts the stream.
startTls :: TLS.TLSParams -> TMVar Stream -> IO (Either XmppFailure ())
startTls params con = Ex.handle (return . Left . TlsError)
                      . flip withStream con
                      . runErrorT $ do
    features <- lift $ gets cFeatures
    state <- gets cState
    case state of
        Plain -> return ()
        Closed -> throwError XmppNoStream
        Secured -> throwError TlsStreamSecured
    con <- lift $ gets cHandle
    when (stls features == Nothing) $ throwError TlsNoServerSupport
    lift $ pushElement starttlsE
    answer <- lift $ pullElement
    case answer of
        Left e -> return $ Left e
        Right (Element "{urn:ietf:params:xml:ns:xmpp-tls}proceed" [] []) -> return $ Right ()
        Right (Element "{urn:ietf:params:xml:ns:xmpp-tls}failure" _ _) -> return $ Left XmppOtherFailure
    (raw, _snk, psh, read, ctx) <- lift $ TLS.tlsinit debug params (mkBackend con)
    let newHand = StreamHandle { cSend = catchPush . psh
                                   , cRecv = read
                                   , cFlush = contextFlush ctx
                                   , cClose = bye ctx >> cClose con
                                   }
    lift $ modify ( \x -> x {cHandle = newHand})
    either (lift . Ex.throwIO) return =<< lift restartStream
    modify (\s -> s{cState = Secured})
    return ()
