{-# LANGUAGE OverloadedStrings #-}


module Network.XMPP.Concurrent
  where

-- import Network.XMPP.Stream
import Network.XMPP.Types

import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.TChan
import Control.Concurrent.STM.TMVar
import Control.Monad.IO.Class
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Resource
import Control.Monad.Trans.State


import qualified Data.ByteString as BS
import Data.Maybe
import Data.IORef

import Network.XMPP.Types
import Network.XMPP.Monad
import Network.XMPP.Marshal
import Network.XMPP.Pickle


import System.IO

import Text.XML.Expat.Format
import Text.XML.Expat.Pickle

data Thread = Thread { messagesRef :: IORef (Maybe (TChan Message))
                     , presenceRef :: IORef (Maybe (TChan Presence))
                     , mShadow :: TChan Stanza -- the original chan
                     , pShadow :: TChan Stanza -- the original chan
                     , outCh :: TChan Stanza
                     }

type XMPPThread a = ReaderT Thread IO a

-- Two streams: input and output. Threads read from input stream and write to output stream.
-- | Runs thread in XmppState monad
-- returns channel of incoming and outgoing stances, respectively
-- and an Action to stop the Threads and close the connection
startThreads :: XMPPMonad (TChan Stanza, TChan Stanza, IO ())
startThreads = do
  writeLock <- liftIO $ newTMVarIO ()
  messagesC <- liftIO newTChanIO
  presenceC <- liftIO newTChanIO
  iqC  <- liftIO newTChanIO
  outC <- liftIO  newTChanIO
  iqHandlers <- liftIO newTVarIO
  pushBS <- gets sConPush
  lw <- liftIO . forkIO $ loopWrite writeLock pushBS outC
  cp <- liftIO . forkIO $ connPersist pushBS writeLock
  s <- get
  rd <- lift . resourceForkIO . void . flip runStateT s . forever $ do
      s <- pull
      case s of
        SMessage  m -> liftIO . atomically $ writeTChan messageC  m
        SPresence p -> liftIO . atomically $ writeTChan presenceC p
        SIQ       i -> liftIO . atomically $ writeTChan presenceC i
  return (inC, outC, killConnection writeLock [lw, rd, cp])
  where
      loopWrite writeLock pushBS out' = forever $ do
                next <- liftIO . atomically $ ( takeTMVar writeLock >> readTChan out')
                liftIO . pushBS . formatNode' $ pickleElem stanzaP next
                liftIO . atomically $ putTMVar writeLock ()
      iqHandler handlers iqC = forever $ do
        iq <- liftIO . atomically $ readTChan iqC


      killConnection writeLock threads = liftIO $ do
        atomically $ takeTMVar writeLock
        forM threads killThread
        return()

runThreaded  :: XMPPThread a
                -> XMPPMonad ThreadId
runThreaded a = do
    (inC, outC, stopThreads) <- startThreads
    workerInCh <- liftIO . newIORef $ Just inC
    worker <- liftIO . forkIO $ do
              runReaderT a (Thread workerInCh inC outC)
              return ()
    return worker


-- | get the inbound stanza channel, duplicate from master if necessary
-- please note that once duplicated it will keep filling up
getInChan = do
  inChR <- asks inChRef
  inCh <- liftIO $ readIORef inChR
  case inCh of
    Nothing -> do
      shadow <- asks shadowInCh
      inCh' <- liftIO $ atomically $ dupTChan shadow
      liftIO $ writeIORef inChR (Just inCh')
      return inCh'
    Just inCh -> return inCh


-- | Drop the local end of the inbound stanza channel
-- from our context so it can be GC-ed
dropInChan :: XMPPThread ()
dropInChan = do
  r <- asks inChRef
  liftIO $ writeIORef r Nothing


-- | Read an element from the inbound stanza channel, acquiring a copy
-- of the channel as necessary
pullS :: XMPPThread Stanza
pullS = do
  c <- getInChan
  st <- liftIO $ atomically $ readTChan c
  return st

-- | Send a stanza to the server
sendS :: Stanza -> XMPPThread ()
sendS a = do
  out <- asks outCh
  liftIO . atomically $ writeTChan out a
  return ()

-- | Fork a new thread
withNewThread :: XMPPThread () -> XMPPThread ThreadId
withNewThread a = do
  thread <- ask
  inCH' <- liftIO $ newIORef Nothing
  liftIO $ forkIO $ runReaderT a (thread {inChRef = inCH'})

waitFor :: (Stanza -> Bool) -> XMPPThread Stanza
waitFor f = do
  s <- pullS
  if (f s) then
    return s
    else do
      waitFor f

connPersist :: (BS.ByteString -> IO ()) -> TMVar () -> IO ()
connPersist pushBS lock = forever $ do
  atomically $ takeTMVar lock
  pushBS " "
  atomically $ putTMVar lock ()
--  putStrLn "<space added>"
  threadDelay 30000000


