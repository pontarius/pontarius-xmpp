module Network.TLSConduit
       ( tlsinit
       , module TLS
       , module TLSExtra
       )
       where

import Control.Applicative
import Control.Monad.Trans

import Crypto.Random

import Data.ByteString
import qualified Data.ByteString.Lazy as BL
import Data.Conduit

import Network.TLS as TLS
import Network.TLS.Extra as TLSExtra

import System.IO(Handle)
import System.Random

tlsinit
  :: (MonadIO m, ResourceIO m1) =>
     TLSParams -> Handle
     -> m (Source m1 ByteString, Sink ByteString m1 ())
tlsinit tlsParams handle = do
    gen <- liftIO $ (newGenIO :: IO SystemRandom) -- TODO: Find better random source?
    clientContext <- client tlsParams gen handle
    handshake clientContext
    let src = sourceIO
               (return clientContext)
               bye
               (\con -> IOOpen <$> recvData con)
    let snk = sinkIO
                (return clientContext)
                (\_ -> return ())
                (\ctx dt -> sendData ctx (BL.fromChunks [dt]) >> return IOProcessing)
                (\_ -> return ())
    return (src, snk)
