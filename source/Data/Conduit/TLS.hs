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
    clientContext <- client tlsParams gen handle
    handshake clientContext
    let src = sourceState
               clientContext
               (\con -> do
                     dt <- recvData con
                     when debug (liftIO $ BS.putStrLn dt)
                     return $ StateOpen con dt)
    let snk = sinkState
         clientContext
         (\con bs -> do
                       sendData con (BL.fromChunks [bs])
                       when debug (liftIO $ BS.putStrLn bs)
                       return (StateProcessing  con))
         (\_ -> return ())
    return ( src
           , snk
           , \s -> do
               when debug (liftIO $ BS.putStrLn s)
               sendData clientContext $ BL.fromChunks [s]
           , clientContext
           )
