{-# OPTIONS_HADDOCK hide #-}
module Network.Xmpp.Concurrent.IQ where

import           Control.Concurrent (forkIO, threadDelay)
import           Control.Concurrent.STM
import           Control.Monad

import qualified Data.Map as Map
import           Data.Text (Text)
import           Data.XML.Types

import           Network.Xmpp.Concurrent.Basic
import           Network.Xmpp.Concurrent.Types
import           Network.Xmpp.Types

-- | Sends an IQ, returns a 'TMVar' that will be filled with the first inbound
-- IQ with a matching ID that has type @result@ or @error@.
sendIQ :: Maybe Int -- ^ Timeout
       -> Maybe Jid -- ^ Recipient (to)
       -> IQRequestType  -- ^ IQ type (@Get@ or @Set@)
       -> Maybe LangTag  -- ^ Language tag of the payload (@Nothing@ for
                         -- default)
       -> Element -- ^ The IQ body (there has to be exactly one)
       -> Session
       -> IO (TMVar IQResponse)
sendIQ timeOut to tp lang body session = do -- TODO: Add timeout
    newId <- idGenerator session
    ref <- atomically $ do
        resRef <- newEmptyTMVar
        (byNS, byId) <- readTVar (iqHandlers session)
        writeTVar (iqHandlers session) (byNS, Map.insert newId resRef byId)
          -- TODO: Check for id collisions (shouldn't happen?)
        return resRef
    sendStanza  (IQRequestS $ IQRequest newId Nothing to lang tp body) session
    case timeOut of
        Nothing -> return ()
        Just t -> void . forkIO $ do
                  threadDelay t
                  doTimeOut (iqHandlers session) newId ref
    return ref
  where
    doTimeOut handlers iqid var = atomically $ do
      p <- tryPutTMVar var IQResponseTimeout
      when p $ do
          (byNS, byId) <- readTVar (iqHandlers session)
          writeTVar handlers (byNS, Map.delete iqid byId)
      return ()


-- | Like 'sendIQ', but waits for the answer IQ. Times out after 3 seconds
sendIQ' :: Maybe Jid
        -> IQRequestType
        -> Maybe LangTag
        -> Element
        -> Session
        -> IO IQResponse
sendIQ' to tp lang body session = do
    ref <- sendIQ (Just 3000000) to tp lang body session
    atomically $ takeTMVar ref


-- | Retrieves an IQ listener channel. If the namespace/'IQRequestType' is not
-- already handled, a new 'TChan' is created and returned as a 'Right' value.
-- Otherwise, the already existing channel will be returned wrapped in a 'Left'
-- value. Note that the 'Left' channel might need to be duplicated in order not
-- to interfere with existing consumers.
listenIQChan :: IQRequestType  -- ^ Type of IQs to receive (@Get@ or @Set@)
             -> Text -- ^ Namespace of the child element
             -> Session
             -> IO (Either (TChan IQRequestTicket) (TChan IQRequestTicket))
listenIQChan tp ns session = do
    let handlers = (iqHandlers session)
    atomically $ do
        (byNS, byID) <- readTVar handlers
        iqCh <- newTChan
        let (present, byNS') = Map.insertLookupWithKey'
                (\_ _ old -> old)
                (tp, ns)
                iqCh
                byNS
        writeTVar handlers (byNS', byID)
        return $ case present of
            Nothing -> Right iqCh
            Just iqCh' -> Left iqCh'

-- | Unregister a previously acquired IQ channel. Please make sure that you
-- where the one who acquired it in the first place as no check for ownership
-- can be made
dropIQChan :: IQRequestType  -- ^ Type of IQ ('Get' or 'Set')
             -> Text -- ^ Namespace of the child element
             -> Session
             -> IO ()
dropIQChan tp ns session = do
    let handlers = (iqHandlers session)
    atomically $ do
        (byNS, byID) <- readTVar handlers
        let byNS' = Map.delete (tp, ns) byNS
        writeTVar handlers (byNS', byID)
        return ()

answerIQ :: IQRequestTicket
         -> Either StanzaError (Maybe Element)
         -> Session
         -> IO Bool
answerIQ (IQRequestTicket
              sRef
              (IQRequest iqid from _to lang _tp bd))
           answer session = do
  let response = case answer of
        Left err  -> IQErrorS $ IQError iqid Nothing from lang err (Just bd)
        Right res -> IQResultS $ IQResult iqid Nothing from lang res
  atomically $ do
       sent <- readTVar sRef
       case sent of
         False -> do
             writeTVar sRef True

             writeTChan (outCh  session) response
             return True
         True -> return False
