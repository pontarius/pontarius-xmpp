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

-- | Buffered source from conduit 0.3
bufferSource :: MonadIO m => Source m o -> IO (Source m o)
bufferSource s = do
    srcRef <- newIORef . Just $ DCI.ResumableSource s (return ())
    return $ do
        src' <- liftIO $ readIORef srcRef
        src <- case src' of
            Just s -> return s
            Nothing -> liftIO $ throwIO SourceClosed
        let go src = do
            (src', res) <- lift $ src $$++ CL.head
            case res of
                Nothing -> liftIO $ writeIORef srcRef Nothing
                Just x -> do
                    liftIO (writeIORef srcRef $ Just src')
                    yield x
                    go src'
          in go src
