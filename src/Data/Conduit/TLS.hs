module Data.Conduit.TLS
       ( tlsinit
       , module TLS
       , module TLSExtra
       )
       where

import Control.Applicative
import Control.Monad.Trans

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
               (\_ -> putStrLn "tls closed")
               (\con -> IOOpen <$> recvData con)
    return (src $= conduitStdout
           , \s -> sendData clientContext $ BL.fromChunks [s] )

-- TODO: remove

conduitStdout :: ResourceIO m
            => Conduit BS.ByteString m BS.ByteString
conduitStdout = conduitIO
    (return ())
    (\_ -> return ())
    (\_ bs -> do
        liftIO $ BS.hPut stdout bs
        return $ IOProducing [bs])
    (const $ return [])