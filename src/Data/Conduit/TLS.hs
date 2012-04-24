{-# Language NoMonomorphismRestriction #-}
{-# OPTIONS_HADDOCK hide #-}
module Data.Conduit.TLS
       ( tlsinit
--       , conduitStdout
       , module TLS
       , module TLSExtra
       )
       where

import Control.Monad(liftM)
import Control.Monad.IO.Class

import Crypto.Random

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.Conduit

import Network.TLS as TLS
import Network.TLS.Extra as TLSExtra

import System.IO(Handle)

tlsinit
  :: (MonadIO m, MonadIO m1) =>
     TLSParams
     -> Handle -> m ( Source m1 BS.ByteString
                    , Sink BS.ByteString m1 ()
                    , BS.ByteString -> IO ()
                    , TLSCtx Handle
                    )
tlsinit tlsParams handle = do
    gen <- liftIO $ (newGenIO :: IO SystemRandom) -- TODO: Find better random source?
    clientContext <- client tlsParams gen handle
    handshake clientContext
    let src = sourceState
               clientContext
               (\con -> StateOpen con `liftM` recvData con)
    let snk = sinkState
         clientContext
         (\con bs -> sendData con (BL.fromChunks [bs])
                     >> return (StateProcessing  con))
         (\_ -> return ())
    return ( src
           , snk
           , \s -> sendData clientContext $ BL.fromChunks [s]
           , clientContext
           )

