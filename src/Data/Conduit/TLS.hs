{-# Language NoMonomorphismRestriction #-}
module Data.Conduit.TLS
       ( tlsinit
--       , conduitStdout
       , module TLS
       , module TLSExtra
       )
       where

import Control.Applicative
import Control.Monad.IO.Class
import Control.Monad.Trans.Resource

import Crypto.Random

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.Conduit

import Network.TLS as TLS
import Network.TLS.Extra as TLSExtra

import System.IO(Handle)

tlsinit
  :: (MonadIO m, MonadIO m1, MonadResource m1) =>
     TLSParams
     -> Handle -> m ( Source m1 BS.ByteString
                    , Sink BS.ByteString m1 ()
                    , BS.ByteString -> IO ())
tlsinit tlsParams handle = do
    gen <- liftIO $ (newGenIO :: IO SystemRandom) -- TODO: Find better random source?
    clientContext <- client tlsParams gen handle
    handshake clientContext
    let src = sourceIO
               (return clientContext)
               (bye)
               (\con -> IOOpen <$> recvData con)
    let snk = sinkIO
         (return clientContext)
         (\_ -> return ())
         (\con bs -> sendData con (BL.fromChunks [bs])
                     >> return IOProcessing )
         (\_ -> return ())
    return ( src
           , snk
           , \s -> sendData clientContext $ BL.fromChunks [s] )

