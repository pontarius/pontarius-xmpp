{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}


module Network.XMPP.Concurrent
  where

-- import Network.XMPP.Stream
import           Network.XMPP.Types

import           Control.Applicative((<$>),(<*>))
import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Concurrent.STM.TChan
import           Control.Concurrent.STM.TMVar
import           Control.Exception (throwTo)
import qualified Control.Exception.Lifted as Ex
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Reader
import           Control.Monad.Trans.Resource
import           Control.Monad.Trans.State


import qualified Data.ByteString as BS
import           Data.Conduit
import qualified Data.Conduit.List as CL
import           Data.Default (def)
import           Data.IORef
import qualified Data.Map as Map
import           Data.Maybe
import           Data.Text(Text)
import           Data.Typeable

import           Data.XML.Types

import           Network.XMPP.Types
import           Network.XMPP.Monad
import           Network.XMPP.Marshal
import           Network.XMPP.Pickle

import           System.IO

import           Text.XML.Stream.Elements
import qualified Text.XML.Stream.Render as XR

data Thread = Thread { messagesRef :: IORef (Maybe (TChan Message))
                     , presenceRef :: IORef (Maybe (TChan Presence))
                     , mShadow :: TChan Message -- the original chan
                     , pShadow :: TChan Presence -- the original chan
                     , outCh :: TChan Stanza
                     , iqHandlers :: TVar ( Map.Map (IQType, Text) (TChan IQ)
                                          , Map.Map Text (TMVar IQ)
                                          )
                     , writeRef :: TMVar (BS.ByteString -> IO () )
                     , readerThread :: ThreadId
                     }

type XMPPThread a = ReaderT Thread IO a


data ReaderSignal = ReaderSignal (XMPPMonad ()) deriving Typeable
instance Show ReaderSignal where show _ = "<ReaderSignal>"
instance Ex.Exception ReaderSignal

readWorker :: TChan Message -> TChan Presence -> TChan IQ -> XMPPState -> ResourceT IO ()
readWorker messageC presenceC iqC s = Ex.catch (forever . flip runStateT s $ do
    sta <- pull
    case sta of
        SMessage  m -> liftIO . atomically $ writeTChan messageC  m
        SPresence p -> liftIO . atomically $ writeTChan presenceC p
        SIQ       i -> liftIO . atomically $ writeTChan iqC i
    )
    ( \(ReaderSignal a) -> do
           ((),s') <- runStateT a s
           readWorker messageC presenceC iqC s'
    )

writeWorker :: TChan Stanza -> TMVar (BS.ByteString -> IO ()) -> IO ()
writeWorker stCh writeRef = forever $ do
  (write, next) <- atomically $ (,) <$>
                     takeTMVar writeRef <*>
                     readTChan stCh
  outBS <- CL.sourceList (elementToEvents $ pickleElem stanzaP next)
             $= XR.renderBytes def $$ CL.consume
  forM outBS write
  atomically $ putTMVar writeRef write


handleIQs handlers iqC = liftIO . forever . atomically $ do
    iq <- readTChan iqC
    (byNS, byID) <- readTVar handlers
    let iqNS' = nameNamespace . elementName . iqBody $ iq
    case iqNS' of
      Nothing -> return () -- TODO: send error stanza
      Just iqNS -> case iqType iq of
         Get -> case Map.lookup (Get, iqNS) byNS of
           Nothing -> return () -- TODO: send error stanza
           Just ch -> writeTChan ch iq
         Set -> case Map.lookup (Set, iqNS) byNS of
           Nothing -> return () -- TODO: send error stanza
           Just ch -> writeTChan ch iq
         Result -> case Map.updateLookupWithKey (\_ _ -> Nothing)
                         (iqId iq) byID of
           (Nothing, _) -> return () -- we are not supposed
                                     -- to send an error
           (Just tmvar, byID')  -> do
               tryPutTMVar tmvar iq -- don't block
               writeTVar handlers (byNS, byID)


-- Two streams: input and output. Threads read from input stream and write to output stream.
-- | Runs thread in XmppState monad
-- returns channel of incoming and outgoing stances, respectively
-- and an Action to stop the Threads and close the connection
startThreads
  :: XMPPMonad ( TChan Message
               , TChan Presence
               , TVar ( Map.Map (IQType, Text) (TChan IQ)
                      , Map.Map Text (TMVar IQ)
                      )
               , TChan Stanza, IO ()
               , TMVar (BS.ByteString -> IO ())
               , ThreadId
               )


startThreads = do
  writeLock <- liftIO . newTMVarIO =<< gets sConPushBS
  messageC <- liftIO newTChanIO
  presenceC <- liftIO newTChanIO
  iqC  <- liftIO newTChanIO
  outC <- liftIO  newTChanIO
  iqHandlers <- liftIO $ newTVarIO ( Map.empty, Map.empty)
  pushEvents <- gets sConPush
  pushBS <- gets sConPushBS
  lw <- liftIO . forkIO $ writeWorker outC writeLock
  cp <- liftIO . forkIO $ connPersist writeLock
  iqh <- liftIO . forkIO $ handleIQs iqHandlers iqC
  s <- get
  rd <- lift . resourceForkIO $ readWorker messageC presenceC iqC s
  return (messageC, presenceC, iqHandlers, outC, killConnection writeLock [lw, rd, cp], writeLock, rd)
  where
      loopWrite writeLock pushEvents out' = forever $ do
                next <- liftIO . atomically $ ( takeTMVar writeLock
                                                >> readTChan out')
                pushEvents . elementToEvents $ pickleElem stanzaP next
                liftIO . atomically $ putTMVar writeLock ()

      killConnection writeLock threads = liftIO $ do
        atomically $ takeTMVar writeLock
        forM threads killThread
        return()

addIQChan :: IQType -> Text -> XMPPThread (Bool, TChan IQ)
addIQChan tp ns = do
  handlers <- asks iqHandlers
  liftIO . atomically $ do
    (byNS, byID) <- readTVar handlers
    iqCh <- newTChan
    let (present, byNS') = Map.insertLookupWithKey' (\_ new _ -> new)
                                                     (tp,ns) iqCh byNS
    writeTVar handlers (byNS', byID)
    return $ case present of
               Nothing -> (False, iqCh)
               Just iqCh' -> (True, iqCh')

runThreaded  :: XMPPThread a
                -> XMPPMonad ThreadId
runThreaded a = do
    (mC, pC, hand, outC, stopThreads, writeR, reader ) <- startThreads
    workermCh <- liftIO . newIORef $ Just mC
    workerpCh <- liftIO . newIORef $ Just pC
    worker <- liftIO . forkIO $ do
              runReaderT a (Thread workermCh workerpCh mC pC outC hand writeR
                            reader)
              return ()
    return worker


-- | get the inbound stanza channel, duplicate from master if necessary
-- please note that once duplicated it will keep filling up
getMessageChan = do
  mChR <- asks messagesRef
  mCh <- liftIO $ readIORef mChR
  case mCh of
    Nothing -> do
      shadow <- asks mShadow
      mCh' <- liftIO $ atomically $ dupTChan shadow
      liftIO $ writeIORef mChR (Just mCh')
      return mCh'
    Just mCh -> return mCh

-- | get the inbound stanza channel, duplicate from master if necessary
-- please note that once duplicated it will keep filling up
getPresenceChan = do
  pChR <- asks presenceRef
  pCh <- liftIO $ readIORef pChR
  case pCh of
    Nothing -> do
      shadow <- asks pShadow
      pCh' <- liftIO $ atomically $ dupTChan shadow
      liftIO $ writeIORef pChR (Just pCh')
      return pCh'
    Just pCh -> return pCh

-- | Drop the local end of the inbound stanza channel
-- from our context so it can be GC-ed
dropMessageChan :: XMPPThread ()
dropMessageChan = do
  r <- asks messagesRef
  liftIO $ writeIORef r Nothing

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


connPersist ::  TMVar (BS.ByteString -> IO ()) -> IO ()
connPersist lock = forever $ do
  pushBS <- atomically $ takeTMVar lock
  pushBS " "
  atomically $ putTMVar lock pushBS
--  putStrLn "<space added>"
  threadDelay 30000000

singleThreaded a = do
  writeLock <- asks writeRef
  reader <- asks readerThread
  liftIO . atomically  $ takeTMVar writeLock
  liftIO . throwTo reader . ReaderSignal $ do
    a
    out <- gets sConPushBS
    liftIO . atomically $ putTMVar writeLock out



