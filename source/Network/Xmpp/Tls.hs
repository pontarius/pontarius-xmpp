{-# OPTIONS_HADDOCK hide #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.Xmpp.Tls where

import qualified Control.Exception.Lifted as Ex
import           Control.Monad
import           Control.Monad.Error
import           Control.Monad.State.Strict
import           Crypto.Random.API
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC8
import qualified Data.ByteString.Lazy as BL
import           Data.Conduit
import           Data.IORef
import           Data.XML.Types
import           Network.TLS
import           Network.Xmpp.Stream
import           Network.Xmpp.Types
import           System.Log.Logger (debugM, errorM)

mkBackend :: StreamHandle -> Backend
mkBackend con = Backend { backendSend = \bs -> void (streamSend con bs)
                        , backendRecv = bufferReceive (streamReceive con)
                        , backendFlush = streamFlush con
                        , backendClose = streamClose con
                        }
  where
    bufferReceive _ 0 = return BS.empty
    bufferReceive recv n = BS.concat `liftM` (go n)
      where
        go n = do
            bs <- recv n
            case BS.length bs of
                0 -> return []
                l -> if l < n
                     then (bs :) `liftM` go (n - l)
                     else return [bs]

starttlsE :: Element
starttlsE = Element "{urn:ietf:params:xml:ns:xmpp-tls}starttls" [] []

-- | Checks for TLS support and run starttls procedure if applicable
tls :: Stream -> IO (Either XmppFailure ())
tls con = Ex.handle (return . Left . TlsError)
                      . flip withStream con
                      . runErrorT $ do
    conf <- gets $ streamConfiguration
    sState <- gets streamConnectionState
    case sState of
        Plain -> return ()
        Closed -> do
            liftIO $ errorM "Pontarius.XMPP" "startTls: The stream is closed."
            throwError XmppNoStream
        Secured -> do
            liftIO $ errorM "Pontarius.XMPP" "startTls: The stream is already secured."
            throwError TlsStreamSecured
    features <- lift $ gets streamFeatures
    case (tlsBehaviour conf, streamTls features) of
        (RequireTls  , Just _   ) -> startTls
        (RequireTls  , Nothing  ) -> throwError TlsNoServerSupport
        (PreferTls   , Just _   ) -> startTls
        (PreferTls   , Nothing  ) -> return ()
        (PreferPlain , Just True) -> startTls
        (PreferPlain , _        ) -> return ()
        (RefuseTls   , Just True) -> throwError XmppOtherFailure
        (RefuseTls   , _        ) -> return ()
  where
    startTls = do
        params <- gets $ tlsParams . streamConfiguration
        sent <- ErrorT $ pushElement starttlsE
        unless sent $ do
            liftIO $ errorM "Pontarius.XMPP" "startTls: Could not sent stanza."
            throwError XmppOtherFailure
        answer <- lift $ pullElement
        case answer of
            Left e -> throwError e
            Right (Element "{urn:ietf:params:xml:ns:xmpp-tls}proceed" [] []) ->
                return ()
            Right (Element "{urn:ietf:params:xml:ns:xmpp-tls}failure" _ _) -> do
                liftIO $ errorM "Pontarius.XMPP" "startTls: TLS initiation failed."
                throwError XmppOtherFailure
            Right r ->
                liftIO $ errorM "Pontarius.XMPP" $
                            "startTls: Unexpected element: " ++ show r
        hand <- gets streamHandle
        (_raw, _snk, psh, recv, ctx) <- lift $ tlsinit params (mkBackend hand)
        let newHand = StreamHandle { streamSend = catchPush . psh
                                   , streamReceive = recv
                                   , streamFlush = contextFlush ctx
                                   , streamClose = bye ctx >> streamClose hand
                                   }
        lift $ modify ( \x -> x {streamHandle = newHand})
        either (lift . Ex.throwIO) return =<< lift restartStream
        modify (\s -> s{streamConnectionState = Secured})
        return ()

client :: (MonadIO m, CPRG rng) => Params -> rng -> Backend -> m Context
client params gen backend  = do
    contextNew backend params gen

xmppDefaultParams :: Params
xmppDefaultParams = defaultParamsClient

tlsinit :: (MonadIO m, MonadIO m1) =>
        TLSParams
     -> Backend
     -> m ( Source m1 BS.ByteString
          , Sink BS.ByteString m1 ()
          , BS.ByteString -> IO ()
          , Int -> m1 BS.ByteString
          , Context
          )
tlsinit params backend = do
    liftIO $ debugM "Pontarius.Xmpp.TLS" "TLS with debug mode enabled."
    gen <- liftIO $ getSystemRandomGen -- TODO: Find better random source?
    con <- client params gen backend
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
    readWithBuffer <- liftIO $ mkReadBuffer (recvData con)
    return ( src
           , snk
           , \s -> do
               liftIO $ debugM "Pontarius.Xmpp.TLS" ("out :" ++ BSC8.unpack s)
               sendData con $ BL.fromChunks [s]
           , liftIO . readWithBuffer
           , con
           )

mkReadBuffer :: IO BS.ByteString -> IO (Int -> IO BS.ByteString)
mkReadBuffer recv = do
    buffer <- newIORef BS.empty
    let read' n = do
            nc <- readIORef buffer
            bs <- if BS.null nc then recv
                                else return nc
            let (result, rest) = BS.splitAt n bs
            writeIORef buffer rest
            return result
    return read'
