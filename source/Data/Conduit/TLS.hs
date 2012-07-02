{-# Language NoMonomorphismRestriction #-}
{-# OPTIONS_HADDOCK hide #-}
module Data.Conduit.TLS
       ( tlsinit
--       , conduitStdout
       , module TLS
       , module TLSExtra
       )
       where

import Control.Monad(liftM, when)
import Control.Monad.IO.Class

import Crypto.Random

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.Conduit
import Control.Monad

import Network.TLS as TLS
import Network.TLS.Extra as TLSExtra

import System.IO(Handle)

tlsinit :: (MonadIO m, MonadIO m1) =>
        Bool
     -> TLSParams
     -> Handle -> m ( Source m1 BS.ByteString
                    , Sink BS.ByteString m1 ()
                    , BS.ByteString -> IO ()
                    , TLSCtx Handle
                    )
tlsinit debug tlsParams handle = do
    when debug . liftIO $ putStrLn "Debug mode enabled"
    gen <- liftIO $ (newGenIO :: IO SystemRandom) -- TODO: Find better random source?
    con <- client tlsParams gen handle
    handshake con
    let src = forever $ do
            dt <- liftIO $ recvData con
            when debug (liftIO $ BS.putStrLn dt)
            yield dt
    let snk = do
            d <- await
            case d of
                Nothing -> return ()
                Just x -> do
                       sendData con (BL.fromChunks [x])
                       when debug (liftIO $ BS.putStrLn x)
                       snk
    return ( src
           , snk
           , \s -> do
               when debug (liftIO $ BS.putStrLn s)
               sendData con $ BL.fromChunks [s]
           , con
           )
