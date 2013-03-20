{-# OPTIONS_HADDOCK hide #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Network.Xmpp.Concurrent.Threads where

import           Control.Applicative((<$>))
import           Control.Concurrent
import           Control.Concurrent.STM
import qualified Control.Exception.Lifted as Ex
import           Control.Monad
import           Control.Monad.Error
import           Control.Monad.State.Strict
import qualified Data.ByteString as BS
import           GHC.IO (unsafeUnmask)
import           Network.Xmpp.Concurrent.Types
import           Network.Xmpp.Stream
import           Network.Xmpp.Types
import           System.Log.Logger

-- Worker to read stanzas from the stream and concurrently distribute them to
-- all listener threads.
readWorker :: (Stanza -> IO ())
           -> (XmppFailure -> IO ())
           -> TMVar Stream
           -> IO a
readWorker onStanza onConnectionClosed stateRef =
    Ex.mask_ . forever $ do
        res <- Ex.catches ( do
                       -- we don't know whether pull will
                       -- necessarily be interruptible
                       s <- atomically $ do
                            s@(Stream con) <- readTMVar stateRef
                            scs <- streamConnectionState <$> readTMVar con
                            when (scs == Closed)
                                 retry
                            return s
                       allowInterrupt
                       Just <$> pullStanza s
                       )
                   [ Ex.Handler $ \(Interrupt t) -> do
                         void $ handleInterrupts [t]
                         return Nothing
                   , Ex.Handler $ \(e :: XmppFailure) -> do
                         onConnectionClosed e
                         errorM "Pontarius.Xmpp" $  "Read error: " ++ show e
                         return Nothing
                   ]
        case res of
              Nothing -> return () -- Caught an exception, nothing to do. TODO: Can this happen?
              Just (Left _) -> return ()
              Just (Right sta) -> onStanza sta
  where
    -- Defining an Control.Exception.allowInterrupt equivalent for GHC 7
    -- compatibility.
    allowInterrupt :: IO ()
    allowInterrupt = unsafeUnmask $ return ()
    -- While waiting for the first semaphore(s) to flip we might receive another
    -- interrupt. When that happens we add it's semaphore to the list and retry
    -- waiting. We do this because we might receive another
    -- interrupt while we're waiting for a mutex to unlock; if that happens, the
    -- new interrupt is added to the list and is waited for as well.
    handleInterrupts :: [TMVar ()] -> IO [()]
    handleInterrupts ts =
        Ex.catch (atomically $ forM ts takeTMVar)
            (\(Interrupt t) -> handleInterrupts (t:ts))

-- Two streams: input and output. Threads read from input stream and write to
-- output stream.
-- | Runs thread in XmppState monad. Returns channel of incoming and outgoing
-- stances, respectively, and an Action to stop the Threads and close the
-- connection.
startThreadsWith :: (Stanza -> IO ())
                 -> TVar EventHandlers
                 -> Stream
                 -> IO (Either XmppFailure (IO (),
                  TMVar (BS.ByteString -> IO Bool),
                  TMVar Stream,
                  ThreadId))
startThreadsWith stanzaHandler eh con = do
    rd <- withStream' (gets $ streamSend . streamHandle >>= \d -> return $ Right d) con
    case rd of
        Left e -> return $ Left e
        Right read' -> do
          writeLock <- newTMVarIO read'
          conS <- newTMVarIO con
          --    lw <- forkIO $ writeWorker outC writeLock
          cp <- forkIO $ connPersist writeLock
          rdw <- forkIO $ readWorker stanzaHandler (noCon eh) conS
          return $ Right ( killConnection writeLock [rdw, cp]
                         , writeLock
                         , conS
                         , rdw
                         )
  where
    killConnection writeLock threads = liftIO $ do
        _ <- atomically $ takeTMVar writeLock -- Should we put it back?
        _ <- forM threads killThread
        return ()
    -- Call the connection closed handlers.
    noCon :: TVar EventHandlers -> XmppFailure -> IO ()
    noCon h e = do
        hands <- atomically $ readTVar h
        _ <- forkIO $ connectionClosedHandler hands e
        return ()

-- Acquires the write lock, pushes a space, and releases the lock.
-- | Sends a blank space every 30 seconds to keep the connection alive.
connPersist :: TMVar (BS.ByteString -> IO Bool) -> IO ()
connPersist lock = forever $ do
    pushBS <- atomically $ takeTMVar lock
    _ <- pushBS " "
    atomically $ putTMVar lock pushBS
    threadDelay 30000000 -- 30s
