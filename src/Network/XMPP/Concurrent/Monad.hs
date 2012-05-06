module Network.XMPP.Concurrent.Monad where

import           Network.XMPP.Types

import           Control.Concurrent
import           Control.Concurrent.STM
import qualified Control.Exception.Lifted as Ex
import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Control.Monad.State.Strict

import           Data.IORef
import qualified Data.Map as Map
import           Data.Text(Text)

import           Network.XMPP.Concurrent.Types
import           Network.XMPP.Monad


-- | Register a new IQ listener. IQ requests matching the type and namespace will
-- be put in the channel.
--
-- Return the new channel or Nothing if this namespace/'IQRequestType'
-- combination was alread handled
listenIQChan :: IQRequestType  -- ^ type of IQs to receive (Get / Set)
                -> Text -- ^ namespace of the child element
                -> XMPP (Maybe ( TChan (IQRequest, TVar Bool)))
listenIQChan tp ns = do
  handlers <- asks iqHandlers
  liftIO . atomically $ do
    (byNS, byID) <- readTVar handlers
    iqCh <- newTChan
    let (present, byNS') = Map.insertLookupWithKey' (\_ _ old -> old)
                                                     (tp,ns) iqCh byNS
    writeTVar handlers (byNS', byID)
    return $ case present of
               Nothing -> Just iqCh
               Just _iqCh' -> Nothing

-- | get the inbound stanza channel, duplicates from master if necessary
-- please note that once duplicated it will keep filling up, call
-- 'dropMessageChan' to allow it to be garbage collected
getMessageChan :: XMPP (TChan (Either MessageError Message))
getMessageChan = do
  mChR <- asks messagesRef
  mCh <- liftIO $ readIORef mChR
  case mCh of
    Nothing -> do
      shadow <- asks mShadow
      mCh' <- liftIO $ atomically $ dupTChan shadow
      liftIO $ writeIORef mChR (Just mCh')
      return mCh'
    Just mCh' -> return mCh'

-- | see 'getMessageChan'
getPresenceChan :: XMPP (TChan (Either PresenceError Presence))
getPresenceChan = do
  pChR <- asks presenceRef
  pCh <- liftIO $ readIORef pChR
  case pCh of
    Nothing -> do
      shadow <- asks pShadow
      pCh' <- liftIO $ atomically $ dupTChan shadow
      liftIO $ writeIORef pChR (Just pCh')
      return pCh'
    Just pCh' -> return pCh'

-- | Drop the local end of the inbound stanza channel
-- from our context so it can be GC-ed
dropMessageChan :: XMPP ()
dropMessageChan = do
  r <- asks messagesRef
  liftIO $ writeIORef r Nothing

-- | see 'dropMessageChan'
dropPresenceChan :: XMPP ()
dropPresenceChan = do
  r <- asks presenceRef
  liftIO $ writeIORef r Nothing

-- | Read an element from the inbound stanza channel, acquiring a copy
-- of the channel as necessary
pullMessage :: XMPP (Either MessageError Message)
pullMessage = do
  c <- getMessageChan
  liftIO $ atomically $ readTChan c

-- | Read an element from the inbound stanza channel, acquiring a copy
-- of the channel as necessary
pullPresence :: XMPP (Either PresenceError Presence)
pullPresence = do
  c <- getPresenceChan
  liftIO $ atomically $ readTChan c

-- | Send a stanza to the server
sendStanza :: Stanza -> XMPP ()
sendStanza a = do
  out <- asks outCh
  liftIO . atomically $ writeTChan out a
  return ()

-- | Create a forked session object without forking a thread
forkSession :: Session -> IO Session
forkSession sess = do
    mCH' <- newIORef Nothing
    pCH' <- newIORef Nothing
    return $ sess {messagesRef = mCH' ,presenceRef = pCH'}

-- | Fork a new thread
fork :: XMPP () -> XMPP ThreadId
fork a = do
  sess <- ask
  sess' <- liftIO $ forkSession sess
  liftIO $ forkIO $ runReaderT a sess'

filterMessages :: (MessageError -> Bool)
               -> (Message -> Bool)
               -> XMPP (Either MessageError Message)
filterMessages f g = do
  s <- pullMessage
  case s of
    Left  e | f e -> return $ Left e
            | otherwise -> filterMessages f g
    Right m | g m -> return $ Right m
            | otherwise -> filterMessages f g

waitForMessage :: (Message -> Bool) -> XMPP Message
waitForMessage f = do
  s <- pullMessage
  case s of
    Left _ -> waitForMessage f
    Right m | f m -> return m
            | otherwise -> waitForMessage f

waitForMessageError :: (MessageError -> Bool) -> XMPP MessageError
waitForMessageError f = do
  s <- pullMessage
  case s of
    Right _ -> waitForMessageError f
    Left  m | f m -> return m
            | otherwise -> waitForMessageError f

waitForPresence :: (Presence -> Bool) -> XMPP Presence
waitForPresence f = do
  s <- pullPresence
  case s of
    Left _ -> waitForPresence f
    Right m | f m -> return m
            | otherwise -> waitForPresence f

-- | Run an XMPPMonad action in isolation.
-- Reader and writer workers will be temporarily stopped
-- and resumed with the new session details once the action returns.
-- The Action will run in the calling thread/
-- Any uncaught exceptions will be interpreted as connection failure
withConnection :: XMPPConMonad a -> XMPP a
withConnection a = do
  readerId <- asks readerThread
  stateRef <- asks conStateRef
  write <- asks writeRef
  wait <- liftIO $ newEmptyTMVarIO
  liftIO . Ex.mask_ $ do
      throwTo readerId $ Interrupt wait
      s <- Ex.catch ( atomically $ do
                         _ <- takeTMVar write
                         s <- takeTMVar stateRef
                         putTMVar wait ()
                         return s
                    )
               (\e -> atomically (putTMVar wait ())
                      >>  Ex.throwIO (e :: Ex.SomeException)
                      -- No MVar taken
               )
      Ex.catch ( do
                   (res, s') <- runStateT a s
                   atomically $ do
                       putTMVar write (sConPushBS s')
                       putTMVar stateRef s'
                   return res
               )
             -- we treat all Exceptions as fatal
             (\e -> runStateT xmppKillConnection s
                     >> Ex.throwIO (e :: Ex.SomeException)
             )

-- | Send a presence Stanza
sendPresence :: Presence -> XMPP ()
sendPresence = sendStanza . PresenceS

-- | Send a Message Stanza
sendMessage :: Message -> XMPP ()
sendMessage = sendStanza . MessageS


modifyHandlers :: (EventHandlers -> EventHandlers) -> XMPP ()
modifyHandlers f = do
    eh <- asks eventHandlers
    liftIO . atomically $ writeTVar eh . f =<< readTVar eh

setSessionEndHandler :: XMPP () -> XMPP ()
setSessionEndHandler eh = do
    r <- ask
    modifyHandlers (\s -> s{sessionEndHandler = runReaderT eh r})

setConnectionClosedHandler :: (StreamError -> XMPP ()) -> XMPP ()
setConnectionClosedHandler eh = do
    r <- ask
    modifyHandlers (\s -> s{connectionClosedHandler = \e -> runReaderT (eh e) r})

-- | run an event handler
runHandler :: (EventHandlers -> IO a) -> XMPP a
runHandler h = do
  eh <- liftIO . atomically . readTVar  =<< asks eventHandlers
  liftIO $ h eh

-- | End the current xmpp session
endSession :: XMPP ()
endSession = do -- TODO: This has to be idempotent (is it?)
    withConnection xmppKillConnection
    liftIO =<< asks stopThreads
    runHandler sessionEndHandler

-- | Close the connection to the server
closeConnection :: XMPP ()
closeConnection = withConnection xmppKillConnection

