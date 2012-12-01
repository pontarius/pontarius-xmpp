{-# LANGUAGE DeriveDataTypeable #-}
module Data.Conduit.BufferedSource where

import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Exception
import Data.IORef
import Data.Conduit
import Data.Typeable(Typeable)
import qualified Data.Conduit.Internal as DCI
import qualified Data.Conduit.List as CL

data SourceClosed = SourceClosed deriving (Show, Typeable)

instance Exception SourceClosed

newtype BufferedSource m o = BufferedSource
                               { bs :: IORef (ResumableSource m o)
                               }


-- | Buffered source from conduit 0.3
bufferSource :: Monad m => Source m o -> IO (BufferedSource m o)
bufferSource s = do
    srcRef <- newIORef $ DCI.ResumableSource s (return ())
    return $ BufferedSource srcRef

(.$$+) (BufferedSource bs) snk = do
    src <- liftIO $ readIORef bs
    (src', r) <- src $$++ snk
    liftIO $ writeIORef bs src'
    return r
