{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Network.Xmpp.Concurrent.Threads where

import Network.Xmpp.Types

import Control.Applicative((<$>))
import Control.Concurrent
import Control.Concurrent.STM
import qualified Control.Exception.Lifted as Ex
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.State.Strict

import qualified Data.ByteString as BS
import Network.Xmpp.Monad
import Network.Xmpp.Concurrent.Types

import GHC.IO (unsafeUnmask)

-- Worker to read stanzas from the stream and concurrently distribute them to
-- all listener threads.
readWorker :: (Stanza -> IO ())
           -> (StreamError -> IO ())
           -> TMVar XmppConnection
           -> IO a
readWorker onStanza onConnectionClosed stateRef =
    Ex.mask_ . forever $ do
        res <- Ex.catches ( do
                       -- we don't know whether pull will
                       -- necessarily be interruptible
                       s <- atomically $ do
                            sr <- readTMVar stateRef
                            when (sConnectionState sr == XmppConnectionClosed)
                                 retry
                            return sr
                       allowInterrupt
                       Just . fst <$> runStateT pullStanza s
                       )
                   [ Ex.Handler $ \(Interrupt t) -> do
                         void $ handleInterrupts [t]
                         return Nothing
                   , Ex.Handler $ \(e :: StreamError) -> do
                         onConnectionClosed e
                         return Nothing
                   ]
        case res of
              Nothing -> return () -- Caught an exception, nothing to do
              Just sta -> onStanza sta
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
                 -> IO
                 (IO (),
                  TMVar (BS.ByteString -> IO Bool),
                  TMVar XmppConnection,
                  ThreadId)
startThreadsWith stanzaHandler eh = do
    writeLock <- newTMVarIO (\_ -> return False)
    conS <- newTMVarIO xmppNoConnection
--    lw <- forkIO $ writeWorker outC writeLock
    cp <- forkIO $ connPersist writeLock
    rd <- forkIO $ readWorker stanzaHandler (noCon eh) conS
    return ( killConnection writeLock [rd, cp]
           , writeLock
           , conS
           , rd
           )
  where
    killConnection writeLock threads = liftIO $ do
        _ <- atomically $ takeTMVar writeLock -- Should we put it back?
        _ <- forM threads killThread
        return ()
    -- Call the connection closed handlers.
    noCon :: TVar EventHandlers -> StreamError -> IO ()
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
