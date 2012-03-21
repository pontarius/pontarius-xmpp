module Data.Conduit.TLS
       ( tlsinit
       , module TLS
       , module TLSExtra
       )
       where

import Control.Applicative
import Control.Monad.IO.Class
import Control.Monad.Trans.Class

import Crypto.Random

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.Conduit

import Network.TLS as TLS
import Network.TLS.Extra as TLSExtra

import System.IO(Handle)
import System.Random

import System.IO

tlsinit
  :: (MonadIO m, ResourceIO m1) =>
     TLSParams -> Handle
     -> m (Source m1 BS.ByteString, (BS.ByteString -> IO ()))
tlsinit tlsParams handle = do
    gen <- liftIO $ (newGenIO :: IO SystemRandom) -- TODO: Find better random source?
    clientContext <- client tlsParams gen handle
    handshake clientContext
    let src = sourceIO
               (return clientContext)
               (bye)
               (\con -> IOOpen <$> recvData con)
    return (src
           , \s -> sendData clientContext $ BL.fromChunks [s] )

-- TODO: remove

conduitStdout :: ResourceIO m
            => Conduit BS.ByteString m BS.ByteString
conduitStdout = conduitIO
    (return ())
    (\_ -> return ())
    (\_ bs -> do
        liftIO $ BS.putStrLn bs
        return $ IOProducing [bs])
    (const $ return [])