module Network.XMPP.Concurrent.Monad where

import           Network.XMPP.Types

import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Reader
import           Control.Monad.Trans.State

import           Data.IORef
import qualified Data.Map as Map
import           Data.Text(Text)

import           Network.XMPP.Concurrent.Types

-- | Register a new IQ listener. IQ matching the type and namespace will
-- be put in the channel. IQ of type Result and Error will never be put
-- into channels, even though this function won't stop you from registering
-- them
listenIQChan :: IQType  -- ^ type of IQs to receive (Get / Set)
                -> Text -- ^ namespace of the child element
                -> XMPPThread (Bool, TChan (IQ, TVar Bool))
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
getMessageChan :: XMPPThread (TChan Message)
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
getPresenceChan :: XMPPThread (TChan Presence)
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
pullMessage :: XMPPThread Message
pullMessage = do
  c <- getMessageChan
  st <- liftIO $ atomically $ readTChan c
  return st

-- | Read an element from the inbound stanza channel, acquiring a copy
-- of the channel as necessary
pullPresence :: XMPPThread Presence
pullPresence = do
  c <- getPresenceChan
  st <- liftIO $ atomically $ readTChan c
  return st


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

waitForMessage :: (Message -> Bool) -> XMPPThread Message
waitForMessage f = do
  s <- pullMessage
  if (f s) then
    return s
    else do
      waitForMessage f

waitForPresence :: (Presence -> Bool) -> XMPPThread Presence
waitForPresence f = do
  s <- pullPresence
  if (f s) then
    return s
    else do
      waitForPresence f

-- | Run an XMPPMonad action in isolation.
-- Reader and writer workers will be temporarily stopped
-- and resumed with the new session details once the action returns.
-- The Action will run in the reader thread.
singleThreaded :: XMPPMonad () -> XMPPThread ()
singleThreaded a = do
  writeLock <- asks writeRef
  rdr <- asks readerThread
  _ <- liftIO . atomically  $ takeTMVar writeLock -- we replace it with the
                                                  -- one returned by a
  liftIO . throwTo rdr . ReaderSignal $ do
    a
    out <- gets sConPushBS
    liftIO . atomically $ putTMVar writeLock out
  return ()

sendPresence :: Presence -> XMPPThread ()
sendPresence = sendS . SPresence

sendMessage :: Message -> XMPPThread ()
sendMessage = sendS . SMessage