{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_HADDOCK hide #-}
module Network.Xmpp.Concurrent.IQ where

import           Control.Applicative ((<$>))
import           Control.Concurrent (forkIO)
import           Control.Concurrent.STM
import           Control.Concurrent.Thread.Delay (delay)
import           Control.Monad
import           Control.Monad.Except
import           Control.Monad.Trans
import qualified Data.Map as Map
import qualified Data.Map.Strict as Map.Strict
import           Data.Maybe
import           Data.Text (Text)
import           Data.XML.Pickle
import           Data.XML.Types
import           Lens.Family2 (toListOf, (&), (^.))
import           Network.Xmpp.Concurrent.Basic
import           Network.Xmpp.Concurrent.Types
import           Network.Xmpp.Lens
import           Network.Xmpp.Stanza
import           Network.Xmpp.Types
import           System.Log.Logger

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
       -> [ExtendedAttribute] -- ^ Additional stanza attributes
       -> Session
       -> IO (Either XmppFailure (STM (Maybe (Annotated IQResponse))))
sendIQ timeOut t tp lang body attrs session = do
    newId <- idGenerator session
    j <- case t of
        Just t -> return $ Right t
        Nothing -> Left <$> getJid session
    ref <- atomically $ do
        resRef <- newEmptyTMVar
        let value = (j, resRef)
        (byNS, byId) <- readTVar (iqHandlers session)
        writeTVar (iqHandlers session) (byNS, Map.insert newId value byId)
        return resRef
    res <- sendStanza (IQRequestS $ IQRequest newId Nothing t lang tp body attrs)
                      session
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
         -> [ExtendedAttribute]
         -> Session
         -> IO (Either IQSendError (Annotated IQResponse))
sendIQA' timeout to tp lang body attrs session = do
    ref <- sendIQ timeout to tp lang body attrs session
    either (return . Left . IQSendError) (fmap (maybe (Left IQTimeOut) Right)
                                     . atomically) ref

-- | Like 'sendIQ', but waits for the answer IQ. Discards plugin Annotations
sendIQ' :: Maybe Integer
        -> Maybe Jid
        -> IQRequestType
        -> Maybe LangTag
        -> Element
        -> [ExtendedAttribute]
        -> Session
        -> IO (Either IQSendError IQResponse)
sendIQ' timeout to tp lang body attrs session =
    fmap fst <$> sendIQA' timeout to tp lang body attrs session

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
        let (present, byNS') = Map.Strict.insertLookupWithKey
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
         -> [ExtendedAttribute]
         -> IO (Maybe (Either XmppFailure ()))
answerIQ = answerTicket


-- Class

class IQRequestClass a where
    data IQResponseType a
    pickleRequest :: PU Element a
    pickleResponse :: PU [Element] (IQResponseType a)
    requestType :: a -> IQRequestType
    requestNamespace :: a -> Text

data IQRequestError = IQRequestSendError XmppFailure
                    | IQRequestTimeout
                    | IQRequestUnpickleError UnpickleError
                      deriving Show

-- | Send an IQ request. May throw IQSendError, UnpickleError,

sendIQRequest  :: (IQRequestClass a, MonadError IQRequestError m, MonadIO m) =>
                  Maybe Integer
               -> Maybe Jid
               -> a
               -> Session
               -> m (Either IQError (IQResponseType a))
sendIQRequest timeout t req con = do
    mbRes <- liftIO $ sendIQ' timeout t (requestType req) Nothing
                              (pickle pickleRequest req) [] con
    case mbRes of
        Left (IQTimeOut) -> throwError IQRequestTimeout
        Left (IQSendError e) -> throwError $ IQRequestSendError e
        Right (IQResponseError e) -> return $ Left e
        Right (IQResponseResult res) ->
              case unpickle pickleResponse (res & toListOf payloadT) of
                   Left e -> throwError $ IQRequestUnpickleError e
                   Right r -> return $ Right r

type IQRequestHandler a = a -> IO (Either StanzaError (IQResponseType a))

runIQHandler :: IQRequestClass a =>
                IQRequestHandler a
             -> Session
             -> IO ()
runIQHandler (handler :: a -> IO (Either StanzaError (IQResponseType a)))
             sess = do
    let prx = undefined :: a
        ns = (requestNamespace prx)
    mbChan <- listenIQ (requestType prx) ns sess
    case mbChan of
        Left _ -> warningM "Pontarius.Xmpp" $ "IQ namespace " ++ show ns
                            ++ " is already handled"
        Right getNext -> forever $ do
            ticket <- atomically getNext
            case unpickle pickleRequest (iqRequestBody ticket ^. payload) of
                Left _ -> answerIQ ticket (Left $ mkStanzaError BadRequest) []
                Right req -> do
                    res <- handler req
                    case res of
                        Left e -> answerIQ ticket (Left e) []
                        Right r -> do
                            let answer = (pickle pickleResponse r)
                            answerIQ ticket (Right $ listToMaybe answer ) []
