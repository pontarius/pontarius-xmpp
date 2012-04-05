{-# LANGUAGE ScopedTypeVariables #-}
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
import qualified Data.Text as Text
import           Data.Text(Text)
import           Data.Typeable

import           Data.XML.Types

import           Network.XMPP.Monad
import           Network.XMPP.Marshal
import           Network.XMPP.Pickle

import           Text.XML.Stream.Elements
import qualified Text.XML.Stream.Render as XR

type IQHandlers = (Map.Map (IQType, Text) (TChan IQ), Map.Map Text (TMVar IQ))

data Thread = Thread { messagesRef :: IORef (Maybe (TChan Message))
                     , presenceRef :: IORef (Maybe (TChan Presence))
                     , mShadow :: TChan Message -- the original chan
                     , pShadow :: TChan Presence -- the original chan
                     , outCh :: TChan Stanza
                     , iqHandlers :: TVar IQHandlers
                     , writeRef :: TMVar (BS.ByteString -> IO () )
                     , readerThread :: ThreadId
                     , idGenerator :: IO Text
                     }

type XMPPThread a = ReaderT Thread IO a


data ReaderSignal = ReaderSignal (XMPPMonad ()) deriving Typeable
instance Show ReaderSignal where show _ = "<ReaderSignal>"
instance Ex.Exception ReaderSignal

readWorker :: TChan Message -> TChan Presence -> TChan IQ -> XMPPState -> ResourceT IO ()
readWorker messageC presenceC iqC s = Ex.catch (forever . flip runStateT s $ do
    sta <- pull
    case sta of
        SMessage  m -> liftIO . atomically $ do
                           writeTChan messageC  m
                           _ <- readTChan messageC -- Sic!
                           return ()
                       -- this may seem ridiculous, but to prevent
                       -- the channel from filling up we immedtiately remove the
                       -- Stanza we just put in. It will still be
                       -- available in duplicates.
        SPresence p -> liftIO . atomically $ do
                           writeTChan presenceC p
                           _ <- readTChan presenceC
                           return ()
        SIQ       i -> liftIO . atomically $ do
                           writeTChan iqC i
                           _ <-readTChan iqC
                           return ()
    )
    ( \(ReaderSignal a) -> do
           ((),s') <- runStateT a s
           readWorker messageC presenceC iqC s'
    )

writeWorker :: TChan Stanza -> TMVar (BS.ByteString -> IO ()) -> IO ()
writeWorker stCh writeR = forever $ do
  (write, next) <- atomically $ (,) <$>
                     takeTMVar writeR <*>
                     readTChan stCh
  outBS <- CL.sourceList (elementToEvents $ pickleElem stanzaP next)
             $= XR.renderBytes def $$ CL.consume
  _ <- forM outBS write
  atomically $ putTMVar writeR write


handleIQs  :: MonadIO m => TVar IQHandlers -> TChan IQ -> m a
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
         -- Result / Error :
         _ -> case Map.updateLookupWithKey (\_ _ -> Nothing)
                         (iqId iq) byID of
           (Nothing, _) -> return () -- we are not supposed
                                     -- to send an error
           (Just tmvar, byID')  -> do
               _ <- tryPutTMVar tmvar iq -- don't block
               writeTVar handlers (byNS, byID')



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
  handlers <- liftIO $ newTVarIO ( Map.empty, Map.empty)
  lw <- liftIO . forkIO $ writeWorker outC writeLock
  cp <- liftIO . forkIO $ connPersist writeLock
  iqh <- liftIO . forkIO $ handleIQs handlers iqC
  s <- get
  rd <- lift . resourceForkIO $ readWorker messageC presenceC iqC s
  return (messageC, presenceC, handlers, outC, killConnection writeLock [lw, rd, cp, iqh], writeLock, rd)
  where
      killConnection writeLock threads = liftIO $ do
        _ <- atomically $ takeTMVar writeLock -- Should we put it back?
        _ <- forM threads killThread
        return()


-- | Register a new IQ listener. IQ matching the type and namespace will
-- be put in the channel. IQ of type Result and Error will never be put
-- into channels, even though this function won't stop you from registering
-- them
listenIQChan :: IQType  -- ^ type of IQs to receive (Get / Set)
                -> Text -- ^ namespace of the child element
                -> XMPPThread (Bool, TChan IQ)
listenIQChan tp ns = do
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

-- | Start worker threads and run action. The supplied action will run
-- in the calling thread. use 'forkXMPP' to start another thread.
runThreaded  :: XMPPThread a
                -> XMPPMonad a
runThreaded a = do
    (mC, pC, hand, outC, _stopThreads, writeR, rdr ) <- startThreads
    workermCh <- liftIO . newIORef $ Nothing
    workerpCh <- liftIO . newIORef $ Nothing
    idRef <- liftIO $ newTVarIO 1
    let getId = atomically $ do
            curId <- readTVar idRef
            writeTVar idRef (curId + 1 :: Integer)
            return . Text.pack $ show curId
    liftIO $ runReaderT a (Thread workermCh workerpCh mC pC outC hand writeR rdr getId)



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


connPersist ::  TMVar (BS.ByteString -> IO ()) -> IO ()
connPersist lock = forever $ do
  pushBS <- atomically $ takeTMVar lock
  pushBS " "
  atomically $ putTMVar lock pushBS
--  putStrLn "<space added>"
  threadDelay 30000000


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

-- | Sends an IQ, returns a 'TMVar' that will be filled with the first inbound
-- IQ with a matching ID that has type @result@ or @error@
sendIQ :: JID -> IQType -> Element -> XMPPThread (TMVar IQ)
sendIQ to tp body = do -- TODO: add timeout
  newId <- liftIO =<< asks idGenerator
  handlers <- asks iqHandlers
  ref <- liftIO . atomically $ do
      resRef <- newEmptyTMVar
      (byNS, byId) <- readTVar handlers
      writeTVar handlers (byNS, Map.insert newId resRef byId)
        -- TODO: Check for id collisions (shouldn't happen?)
      return resRef
  sendS . SIQ $ IQ Nothing (Just to) newId tp body
  return ref


