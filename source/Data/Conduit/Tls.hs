{-# Language NoMonomorphismRestriction #-}
{-# OPTIONS_HADDOCK hide #-}
module Data.Conduit.Tls
       ( tlsinit
--       , conduitStdout
       , module TLS
       , module TLSExtra
       )
       where

import           Control.Monad
import           Control.Monad (liftM, when)
import           Control.Monad.IO.Class

import           Crypto.Random

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import           Data.Conduit
import qualified Data.Conduit.Binary as CB
import Data.IORef

import           Network.TLS as TLS
import           Crypto.Random.API
import           Network.TLS.Extra as TLSExtra

import           System.IO (Handle)

client params gen backend  = do
    contextNew backend params gen

defaultParams = defaultParamsClient

tlsinit :: (MonadIO m, MonadIO m1) =>
        Bool
     -> TLSParams
     -> Backend
     -> m ( Source m1 BS.ByteString
          , Sink BS.ByteString m1 ()
          , BS.ByteString -> IO ()
          , Int -> m1 BS.ByteString
          , Context
          )
tlsinit debug tlsParams backend = do
    when debug . liftIO $ putStrLn "TLS with debug mode enabled"
    gen <- liftIO $ getSystemRandomGen -- TODO: Find better random source?
    con <- client tlsParams gen backend
    handshake con
    let src = forever $ do
            dt <- liftIO $ recvData con
            when debug (liftIO $ putStr "in: " >> BS.putStrLn dt)
            yield dt
    let snk = do
            d <- await
            case d of
                Nothing -> return ()
                Just x -> do
                       sendData con (BL.fromChunks [x])
                       when debug (liftIO $ putStr "out: " >>  BS.putStrLn x)
                       snk
    read <- liftIO $ mkReadBuffer (recvData con)
    return ( src
           , snk
           , \s -> do
               when debug (liftIO $ BS.putStrLn s)
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
