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
import qualified Data.ByteString.Char8 as BSC8
import           Data.Conduit
import qualified Data.Conduit.Binary as CB
import           Data.Typeable
import           Data.XML.Types

import           Network.Xmpp.Stream
import           Network.Xmpp.Types
import           System.Log.Logger

import           Control.Concurrent.STM.TMVar

import           Data.IORef
import           Crypto.Random.API
import           Network.TLS
import           Network.TLS.Extra

mkBackend con = Backend { backendSend = \bs -> void (streamSend con bs)
                        , backendRecv = streamReceive con
                        , backendFlush = streamFlush con
                        , backendClose = streamClose con
                        }

starttlsE :: Element
starttlsE = Element "{urn:ietf:params:xml:ns:xmpp-tls}starttls" [] []

-- Pushes "<starttls/>, waits for "<proceed/>", performs the TLS handshake, and
-- restarts the stream.
startTls :: TMVar Stream -> IO (Either XmppFailure ())
startTls con = Ex.handle (return . Left . TlsError)
                      . flip withStream con
                      . runErrorT $ do
    lift $ lift $ debugM "Pontarius.XMPP" "startTls: Securing stream..."
    features <- lift $ gets streamFeatures
    config <- lift $ gets streamConfiguration
    let params = tlsParams config
    state <- gets streamState
    case state of
        Plain -> return ()
        Closed -> do
            lift $ lift $ errorM "Pontarius.XMPP" "startTls: The stream is closed."
            throwError XmppNoStream
        Secured -> do
            lift $ lift $ errorM "Pontarius.XMPP" "startTls: The stream is already secured."
            throwError TlsStreamSecured
    con <- lift $ gets streamHandle
    when (streamTls features == Nothing) $ do 
        lift $ lift $ errorM "Pontarius.XMPP" "The server does not support TLS."
        throwError TlsNoServerSupport
    lift $ pushElement starttlsE
    answer <- lift $ pullElement
    case answer of
        Left e -> return $ Left e
        Right (Element "{urn:ietf:params:xml:ns:xmpp-tls}proceed" [] []) -> return $ Right ()
        Right (Element "{urn:ietf:params:xml:ns:xmpp-tls}failure" _ _) -> do
            lift $ lift $ errorM "Pontarius.XMPP" "startTls: TLS initiation failed."
            return . Left $ XmppOtherFailure
    (raw, _snk, psh, read, ctx) <- lift $ tlsinit params (mkBackend con)
    let newHand = StreamHandle { streamSend = catchPush . psh
                                   , streamReceive = read
                                   , streamFlush = contextFlush ctx
                                   , streamClose = bye ctx >> streamClose con
                                   }
    lift $ modify ( \x -> x {streamHandle = newHand})
    either (lift . Ex.throwIO) return =<< lift restartStream
    modify (\s -> s{streamState = Secured})
    lift $ lift $ debugM "Pontarius.XMPP" "startTls: Stream secured."
    return ()

client params gen backend  = do
    contextNew backend params gen

defaultParams = defaultParamsClient

tlsinit :: (MonadIO m, MonadIO m1) =>
        TLSParams
     -> Backend
     -> m ( Source m1 BS.ByteString
          , Sink BS.ByteString m1 ()
          , BS.ByteString -> IO ()
          , Int -> m1 BS.ByteString
          , Context
          )
tlsinit tlsParams backend = do
    liftIO $ debugM "Pontarius.Xmpp.TLS" "TLS with debug mode enabled."
    gen <- liftIO $ getSystemRandomGen -- TODO: Find better random source?
    con <- client tlsParams gen backend
    handshake con
    let src = forever $ do
            dt <- liftIO $ recvData con
            liftIO $ debugM "Pontarius.Xmpp.TLS" ("in :" ++ BSC8.unpack dt)
            yield dt
    let snk = do
            d <- await
            case d of
                Nothing -> return ()
                Just x -> do
                       sendData con (BL.fromChunks [x])
                       liftIO $ debugM "Pontarius.Xmpp.TLS"
                                       ("out :" ++ BSC8.unpack x)
                       snk
    read <- liftIO $ mkReadBuffer (recvData con)
    return ( src
           , snk
           , \s -> do
               liftIO $ debugM "Pontarius.Xmpp.TLS" ("out :" ++ BSC8.unpack s)
               sendData con $ BL.fromChunks [s]
           , liftIO . read
           , con
           )

mkReadBuffer :: IO BS.ByteString -> IO (Int -> IO BS.ByteString)
mkReadBuffer read = do
    buffer <- newIORef BS.empty
    let read' n = do
            nc <- readIORef buffer
            bs <- if BS.null nc then read
                                else return nc
            let (result, rest) = BS.splitAt n bs
            writeIORef buffer rest
            return result
    return read'
