module Data.Conduit.BufferedSource where

import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Data.IORef
import Data.Conduit
import qualified Data.Conduit.List as CL

-- | Buffered source from conduit 0.3
bufferSource :: MonadIO m => Source m o -> IO (Source m o)
bufferSource s = do
    srcRef <- newIORef s
    return $ do
        src <- liftIO $ readIORef srcRef
        let go src = do
            (src', res) <- lift $ src $$+ CL.head
            case res of
                Nothing -> return ()
                Just x -> liftIO (writeIORef srcRef src') >> yield x >> go src'
          in go src
