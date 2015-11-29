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
import qualified Data.ByteString as BS
import           GHC.IO (unsafeUnmask)
import           Network.Xmpp.Concurrent.Types
import           Network.Xmpp.Stream
import           Network.Xmpp.Types
import           System.Log.Logger

-- Worker to read stanzas from the stream and concurrently distribute them to
-- all listener threads.
readWorker :: (XmppElement -> IO ())
           -> (XmppFailure -> IO ())
           -> TMVar Stream
           -> IO a
readWorker onElement onCClosed stateRef = forever . Ex.mask_ $ do
    s' <- Ex.catches ( do
                        atomically $ do
                            s@(Stream con) <- readTMVar stateRef
                            scs <- streamConnectionState <$> readTMVar con
                            when (stateIsClosed scs)
                                 retry
                            return $ Just s
                   )
               [ Ex.Handler $ \(Interrupt t) -> do
                     void $ handleInterrupts [t]
                     return Nothing

               ]
    case s' of -- Maybe Stream
        Nothing -> return ()
        Just s -> do -- Stream
            res <- Ex.catches (do
                   -- we don't know whether pull will
                   -- necessarily be interruptible
                             allowInterrupt
                             res <- pullXmppElement s
                             case res of
                                 Left e -> do
                                     errorM "Pontarius.Xmpp" $ "Read error: "
                                         ++ show e
                                     _ <- closeStreams s
                                     onCClosed e
                                     return Nothing
                                 Right r -> return $ Just r
                              )
                       [ Ex.Handler $ \(Interrupt t) -> do
                              void $ handleInterrupts [t]
                              return Nothing
                       ]
            case res of
                Nothing -> return () -- Caught an exception, nothing to
                                     -- do. TODO: Can this happen?
                Just sta -> void $ onElement sta
  where
    -- Defining an Control.Exception.allowInterrupt equivalent for GHC 7
    -- compatibility.
    allowInterrupt :: IO ()
    allowInterrupt = unsafeUnmask $ return ()
    -- While waiting for the first semaphore(s) to flip we might receive another
    -- interrupt. When that happens we add it's semaphore to the list and retry
    -- waiting.
    handleInterrupts :: [TMVar ()] -> IO [()]
    handleInterrupts ts =
        Ex.catch (atomically $ forM ts takeTMVar)
            (\(Interrupt t) -> handleInterrupts (t:ts))
    stateIsClosed Closed   = True
    stateIsClosed Finished = True
    stateIsClosed _        = False

-- | Runs thread in XmppState monad. Returns channel of incoming and outgoing
-- stances, respectively, and an Action to stop the Threads and close the
-- connection.
startThreadsWith :: TMVar (BS.ByteString -> IO (Either XmppFailure ()))
                 -> (XmppElement -> IO ())
                 -> TMVar EventHandlers
                 -> Stream
                 -> Maybe Int
                 -> IO (Either XmppFailure (IO (),
                                            TMVar Stream,
                                            ThreadId))
startThreadsWith writeSem stanzaHandler eh con keepAlive = do
    -- read' <- withStream' (gets $ streamSend . streamHandle) con
    -- writeSem <- newTMVarIO read'
    conS <- newTMVarIO con
    cp <- forkIO $ connPersist keepAlive writeSem
    let onConClosed failure = do
            stopWrites
            noCon eh failure
    rdw <- forkIO $ readWorker stanzaHandler onConClosed conS
    return $ Right ( killConnection [rdw, cp]
                   , conS
                   , rdw
                   )
  where
    stopWrites = atomically $ do
        _ <- takeTMVar writeSem
        putTMVar writeSem $ \_ -> return $ Left XmppNoStream
    killConnection threads = liftIO $ do
        debugM "Pontarius.Xmpp" "killing connection"
        stopWrites
        debugM "Pontarius.Xmpp" "killing threads"
        _ <- forM threads killThread
        return ()
    -- Call the connection closed handlers.
    noCon :: TMVar EventHandlers -> XmppFailure -> IO ()
    noCon h e = do
        hands <- atomically $ readTMVar h
        _ <- forkIO $ connectionClosedHandler hands e
        return ()

-- Acquires the write lock, pushes a space, and releases the lock.
-- | Sends a blank space every <delay> seconds to keep the connection alive.
connPersist :: Maybe Int -> TMVar (BS.ByteString -> IO a) -> IO ()
connPersist (Just delay) sem = forever $ do
    pushBS <- atomically $ takeTMVar sem
    _ <- pushBS " "
    atomically $ putTMVar sem pushBS
    threadDelay (delay*1000000)
connPersist Nothing _ = return ()
