{-# OPTIONS_HADDOCK hide #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Network.Xmpp.Concurrent.Threads where

import           Network.Xmpp.Types

import           Control.Applicative((<$>))
import           Control.Concurrent
import           Control.Concurrent.STM
import qualified Control.Exception.Lifted as Ex
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.State.Strict

import qualified Data.ByteString as BS
import           Network.Xmpp.Concurrent.Types
import           Network.Xmpp.Stream

import           Control.Concurrent.STM.TMVar

import           GHC.IO (unsafeUnmask)

import           Control.Monad.Error

-- Worker to read stanzas from the stream and concurrently distribute them to
-- all listener threads.
readWorker :: (Stanza -> IO ())
           -> (XmppFailure -> IO ())
           -> TMVar (TMVar Stream)
           -> IO a
readWorker onStanza onConnectionClosed stateRef =
    Ex.mask_ . forever $ do
        res <- Ex.catches ( do
                       -- we don't know whether pull will
                       -- necessarily be interruptible
                       s <- atomically $ do
                            con <- readTMVar stateRef
                            state <- cState <$> readTMVar con
                            when (state == Closed)
                                 retry
                            return con
                       allowInterrupt
                       Just <$> pullStanza s
                       )
                   [ Ex.Handler $ \(Interrupt t) -> do
                         void $ handleInterrupts [t]
                         return Nothing
                   , Ex.Handler $ \(e :: XmppFailure) -> do
                         onConnectionClosed e
                         return Nothing
                   ]
        case res of
              Nothing -> return () -- Caught an exception, nothing to do. TODO: Can this happen?
              Just (Left e) -> return ()
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
                 -> TMVar Stream
                 -> IO (Either XmppFailure (IO (),
                  TMVar (BS.ByteString -> IO Bool),
                  TMVar (TMVar Stream),
                  ThreadId))
startThreadsWith stanzaHandler eh con = do
    read <- withStream' (gets $ cSend . cHandle >>= \d -> return $ Right d) con
    case read of
        Left e -> return $ Left e
        Right read' -> do
          writeLock <- newTMVarIO read'
          conS <- newTMVarIO con
          --    lw <- forkIO $ writeWorker outC writeLock
          cp <- forkIO $ connPersist writeLock
          rd <- forkIO $ readWorker stanzaHandler (noCon eh) conS
          return $ Right ( killConnection writeLock [rd, cp]
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
