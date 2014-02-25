{-# OPTIONS_HADDOCK hide #-}
{-# LANGUAGE OverloadedStrings #-}
module Network.Xmpp.Concurrent.Monad where

import           Control.Applicative ((<$>))
import           Control.Concurrent
import           Control.Concurrent.STM
import qualified Control.Exception.Lifted as Ex
import           Control.Monad.State
import           Network.Xmpp.Concurrent.Types
import           Network.Xmpp.Stream
import           Network.Xmpp.Types

-- TODO: Wait for presence error?

-- | Run an XmppConMonad action in isolation. Reader and writer workers will be
-- temporarily stopped and resumed with the new session details once the action
-- returns. The action will run in the calling thread. Any uncaught exceptions
-- will be interpreted as connection failure.
-- withConnection :: XmppConMonad a -> Context -> IO (Either StreamError a)
withConnection :: (Stream -> IO (b, Stream))
               -> Session
               -> IO (Either XmppFailure b)
withConnection a session =  do
    wait <- newEmptyTMVarIO
    Ex.mask_ $ do
        -- Suspends the reader until the lock (wait) is released (set to `()').
        throwTo (readerThread session) $ Interrupt wait
        -- We acquire the write and stateRef locks, to make sure that this is
        -- the only thread that can write to the stream and to perform a
        -- withConnection calculation. Afterwards, we release the lock and
        -- fetches an updated state.
        s <- Ex.catch
            (atomically $ do
                 _ <- takeTMVar (writeSemaphore session)
                 s <- takeTMVar (streamRef session)
                 putTMVar wait ()
                 return s
            )
            -- If we catch an exception, we have failed to take the MVars above.
            (\e -> atomically (putTMVar wait ()) >>
                 Ex.throwIO (e :: Ex.SomeException)
            )
        -- Run the XmppMonad action, save the (possibly updated) states, release
        -- the locks, and return the result.
        Ex.catches
            (do
                 (res, s') <- a s
                 wl <- withStream' (gets $ streamSend . streamHandle) s'
                 atomically $ do
                     putTMVar (writeSemaphore session) wl
                     putTMVar (streamRef session) s'
                     return $ Right res
            ) -- TODO: DO we have to replace the MVars in case of ane exception?
            -- We treat all Exceptions as fatal. If we catch a StreamError, we
            -- return it. Otherwise, we throw an exception.
            [ Ex.Handler $ \e -> return $ Left (e :: XmppFailure)
            , Ex.Handler $ \e -> killStream s
                  >> Ex.throwIO (e :: Ex.SomeException)
            ]

-- | Executes a function to update the event handlers.
modifyHandlers :: (EventHandlers -> EventHandlers) -> Session -> IO ()
modifyHandlers f session = atomically $ modifyTMVar_ (eventHandlers session) f
  where
    -- Borrowing modifyTVar from
    -- http://hackage.haskell.org/packages/archive/stm/2.4/doc/html/src/Control-Concurrent-STM-TVar.html
    -- as it's not available in GHC 7.0.
    modifyTMVar_ :: TMVar a -> (a -> a) -> STM ()
    modifyTMVar_ var g = do
      x <- takeTMVar var
      putTMVar var (g x)

-- | Changes the handler to be executed when the server connection is closed. To
-- avoid race conditions the initial value should be set in the configuration
-- when creating the session
setConnectionClosedHandler :: (XmppFailure -> Session -> IO ()) -> Session -> IO ()
setConnectionClosedHandler eh session = do
    modifyHandlers (\s -> s{connectionClosedHandler =
                                 \e -> eh e session}) session

runConnectionClosedHandler :: Session -> XmppFailure -> IO ()
runConnectionClosedHandler session e = do
    h <- connectionClosedHandler <$> atomically (readTMVar
                                                  $ eventHandlers session)
    h e

-- | Run an event handler.
runHandler :: (EventHandlers -> IO a) -> Session -> IO a
runHandler h session = h =<< atomically (readTMVar $ eventHandlers session)


-- | End the current XMPP session. Kills the associated threads and closes the
-- connection.
--
-- Note that XMPP clients (that have signalled availability) should send
-- \"Unavailable\" presence prior to disconnecting.
--
-- The connectionClosedHandler will not be called (to avoid possibly
-- reestablishing the connection).
endSession :: Session -> IO ()
endSession session =  do -- TODO: This has to be idempotent (is it?)
    stopThreads session
    _ <- flip withConnection session $ \stream -> do
        _ <- closeStreams stream
        return ((), stream)
    return ()


-- | Close the connection to the server. Closes the stream (by enforcing a
-- write lock and sending a \</stream:stream\> element), waits (blocks) for
-- three seconds, and then closes the connection.
closeConnection :: Session -> IO ()
closeConnection session = do
    _ <-flip withConnection session $ \stream -> do
        _ <- closeStreams stream
        return ((), stream)
    runConnectionClosedHandler session StreamEndFailure
