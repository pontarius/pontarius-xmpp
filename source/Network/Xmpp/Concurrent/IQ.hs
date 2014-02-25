{-# OPTIONS_HADDOCK hide #-}
module Network.Xmpp.Concurrent.IQ where

import           Control.Applicative ((<$>))
import           Control.Concurrent (forkIO)
import           Control.Concurrent.STM
import           Control.Concurrent.Thread.Delay (delay)
import           Control.Monad
import qualified Data.Map as Map
import           Data.Text (Text)
import           Data.XML.Types
import           Network.Xmpp.Concurrent.Basic
import           Network.Xmpp.Concurrent.Types
import           Network.Xmpp.Types

-- | Sends an IQ, returns an STM action that returns the first inbound IQ with a
-- matching ID that has type @result@ or @error@ or Nothing if the timeout was
-- reached.
--
-- When sending the action fails, an XmppFailure is returned.
sendIQ :: Maybe Integer -- ^ Timeout . When the timeout is reached the response
                        -- TMVar will be filled with 'IQResponseTimeout' and the
                        -- id is removed from the list of IQ handlers. 'Nothing'
                        -- deactivates the timeout
       -> Maybe Jid -- ^ Recipient (to)
       -> IQRequestType  -- ^ IQ type (@Get@ or @Set@)
       -> Maybe LangTag  -- ^ Language tag of the payload (@Nothing@ for
                         -- default)
       -> Element -- ^ The IQ body (there has to be exactly one)
       -> Session
       -> IO (Either XmppFailure (STM (Maybe (Annotated IQResponse))))
sendIQ timeOut to tp lang body session = do
    newId <- idGenerator session
    j <- case to of
        Just t -> return $ Right t
        Nothing -> Left <$> getJid session
    ref <- atomically $ do
        resRef <- newEmptyTMVar
        let value = (j, resRef)
        (byNS, byId) <- readTVar (iqHandlers session)
        writeTVar (iqHandlers session) (byNS, Map.insert newId value byId)
        return resRef
    res <- sendStanza (IQRequestS $ IQRequest newId Nothing to lang tp body) session
    case res of
        Right () -> do
            case timeOut of
                Nothing -> return ()
                Just t -> void . forkIO $ do
                          delay t
                          doTimeOut (iqHandlers session) newId ref
            return . Right $ readTMVar ref
        Left e -> return $ Left e
  where
    doTimeOut handlers iqid var = atomically $ do
      p <- tryPutTMVar var Nothing
      when p $ do
          (byNS, byId) <- readTVar (iqHandlers session)
          writeTVar handlers (byNS, Map.delete iqid byId)
      return ()

-- | Like 'sendIQ', but waits for the answer IQ.
sendIQA' :: Maybe Integer
        -> Maybe Jid
        -> IQRequestType
        -> Maybe LangTag
        -> Element
        -> Session
        -> IO (Either IQSendError (Annotated IQResponse))
sendIQA' timeout to tp lang body session = do
    ref <- sendIQ timeout to tp lang body session
    either (return . Left . IQSendError) (fmap (maybe (Left IQTimeOut) Right)
                                     . atomically) ref

-- | Like 'sendIQ', but waits for the answer IQ. Discards plugin Annotations
sendIQ' :: Maybe Integer
        -> Maybe Jid
        -> IQRequestType
        -> Maybe LangTag
        -> Element
        -> Session
        -> IO (Either IQSendError IQResponse)
sendIQ' timeout to tp lang body session = fmap fst <$> sendIQA' timeout to tp lang body session

-- | Register your interest in inbound IQ stanzas of a specific type and
-- namespace. The returned STM action yields the received, matching IQ stanzas.
--
-- If a handler for IQ stanzas with the given type and namespace is already
-- registered, the producer will be wrapped in Left. In this case the returned
-- request tickets may already be processed elsewhere.
listenIQ :: IQRequestType  -- ^ Type of IQs to receive ('Get' or 'Set')
         -> Text -- ^ Namespace of the child element
         -> Session
         -> IO (Either (STM IQRequestTicket) (STM IQRequestTicket))
listenIQ tp ns session = do
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
        case present of
            Nothing -> return . Right $ readTChan iqCh
            Just iqCh' -> do
                clonedChan <- cloneTChan iqCh'
                return . Left $ readTChan clonedChan


-- | Unregister a previously registered IQ handler. No more IQ stanzas will be
-- delivered to any of the returned producers.
unlistenIQ :: IQRequestType  -- ^ Type of IQ ('Get' or 'Set')
           -> Text -- ^ Namespace of the child element
           -> Session
           -> IO ()
unlistenIQ tp ns session = do
    let handlers = (iqHandlers session)
    atomically $ do
        (byNS, byID) <- readTVar handlers
        let byNS' = Map.delete (tp, ns) byNS
        writeTVar handlers (byNS', byID)
        return ()

-- | Answer an IQ request. Only the first answer ist sent and Just True is
-- returned when the answer is sucessfully sent. If an error occured during
-- sending Just False is returned (and another attempt can be
-- undertaken). Subsequent answers after the first sucessful one are dropped and
-- (False is returned in that case)
answerIQ :: IQRequestTicket
         -> Either StanzaError (Maybe Element)
         -> IO (Maybe (Either XmppFailure ()))
answerIQ ticket = answerTicket ticket
