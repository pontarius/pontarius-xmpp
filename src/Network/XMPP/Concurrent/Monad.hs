module Network.XMPP.Concurrent.Monad where

import           Network.XMPP.Types

import           Control.Concurrent
import           Control.Concurrent.STM
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
listenIQChan :: IQRequestType  -- ^ type of IQs to receive (Get / Set)
                -> Text -- ^ namespace of the child element
                -> XMPPThread (Bool, TChan (IQRequest, TVar Bool))
listenIQChan tp ns = do
  handlers <- asks iqHandlers
  liftIO . atomically $ do
    (byNS, byID) <- readTVar handlers
    iqCh <- newTChan
    let (present, byNS') = Map.insertLookupWithKey' (\_ new _ -> new)
                                                     (tp,ns) iqCh byNS
    writeTVar handlers (byNS', byID)
    return $ case present of
               Nothing -> (True, iqCh)
               Just iqCh' -> (False, iqCh')

-- | get the inbound stanza channel, duplicates from master if necessary
-- please note that once duplicated it will keep filling up, call
-- 'dropMessageChan' to allow it to be garbage collected
getMessageChan :: XMPPThread (TChan (Either MessageError Message))
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
getPresenceChan :: XMPPThread (TChan (Either PresenceError Presence))
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
dropMessageChan :: XMPPThread ()
dropMessageChan = do
  r <- asks messagesRef
  liftIO $ writeIORef r Nothing

-- | see 'dropMessageChan'
dropPresenceChan :: XMPPThread ()
dropPresenceChan = do
  r <- asks presenceRef
  liftIO $ writeIORef r Nothing

-- | Read an element from the inbound stanza channel, acquiring a copy
-- of the channel as necessary
pullMessage :: XMPPThread (Either MessageError Message)
pullMessage = do
  c <- getMessageChan
  liftIO $ atomically $ readTChan c

-- | Read an element from the inbound stanza channel, acquiring a copy
-- of the channel as necessary
pullPresence :: XMPPThread (Either PresenceError Presence)
pullPresence = do
  c <- getPresenceChan
  liftIO $ atomically $ readTChan c

-- | Send a stanza to the server
sendS :: Stanza -> XMPPThread ()
sendS a = do
  out <- asks outCh
  liftIO . atomically $ writeTChan out a
  return ()

-- | Fork a new thread
forkXMPP :: XMPPThread () -> XMPPThread ThreadId
forkXMPP a = do
  thread <- ask
  mCH' <- liftIO $ newIORef Nothing
  pCH' <- liftIO $ newIORef Nothing
  liftIO $ forkIO $ runReaderT a (thread {messagesRef = mCH'
                                         ,presenceRef = pCH'
                                         })

filterMessages :: (MessageError -> Bool)
               -> (Message -> Bool)
               -> XMPPThread (Either MessageError Message)
filterMessages f g = do
  s <- pullMessage
  case s of
    Left  e | f e -> return $ Left e
            | otherwise -> filterMessages f g
    Right m | g m -> return $ Right m
            | otherwise -> filterMessages f g

waitForMessage :: (Message -> Bool) -> XMPPThread Message
waitForMessage f = do
  s <- pullMessage
  case s of
    Left _ -> waitForMessage f
    Right m | f m -> return m
            | otherwise -> waitForMessage f

waitForMessageError :: (MessageError -> Bool) -> XMPPThread MessageError
waitForMessageError f = do
  s <- pullMessage
  case s of
    Right _ -> waitForMessageError f
    Left  m | f m -> return m
            | otherwise -> waitForMessageError f

waitForPresence :: (Presence -> Bool) -> XMPPThread Presence
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
-- NB: This will /not/ catch any exceptions. If you action dies, deadlocks
-- or otherwisely exits abnormaly the XMPP session will be dead.
withConnection :: XMPPConMonad a -> XMPPThread a
withConnection a = do
  readerId <- asks readerThread
  stateRef <- asks conStateRef
  write <- asks writeRef
  wait <- liftIO $ newEmptyTMVarIO
  liftIO . throwTo readerId $ Interrupt wait
  s <- liftIO . atomically $ do
    putTMVar wait ()
    _ <- takeTMVar write
    takeTMVar stateRef
  (res, s') <- liftIO $ runStateT a s
  liftIO . atomically $ do
    putTMVar write (sConPushBS s')
    putTMVar stateRef s'
  return res

-- | Send a presence Stanza
sendPresence :: Presence -> XMPPThread ()
sendPresence = sendS . PresenceS

-- | Send a Message Stanza
sendMessage :: Message -> XMPPThread ()
sendMessage = sendS . MessageS


modifyHandlers :: (EventHandlers -> EventHandlers) -> XMPPThread ()
modifyHandlers f = do
    eh <- asks eventHandlers
    liftIO . atomically $ modifyTVar eh f

setSessionEndHandler :: XMPPThread () -> XMPPThread ()
setSessionEndHandler eh = modifyHandlers (\s -> s{sessionEndHandler = eh})

-- | run an event handler
runHandler :: (EventHandlers -> XMPPThread a) -> XMPPThread a
runHandler h = do
  eh <- liftIO . atomically . readTVar  =<< asks eventHandlers
  h eh

-- | End the current xmpp session
endSession :: XMPPThread ()
endSession = do -- TODO: This has to be idempotent (is it?)
    withConnection xmppKillConnection
    liftIO =<< asks stopThreads
    runHandler sessionEndHandler

-- | Close the connection to the server
closeConnection :: XMPPThread ()
closeConnection = withConnection xmppKillConnection

