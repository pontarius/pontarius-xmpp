{-# OPTIONS_HADDOCK hide #-}
module Network.Xmpp.Concurrent.IQ where

import           Control.Concurrent (forkIO)
import           Control.Concurrent.Thread.Delay (delay)
import           Control.Concurrent.STM

import           Control.Monad

import qualified Data.Map as Map
import           Data.Text (Text)
import           Data.XML.Types

import           Network.Xmpp.Concurrent.Basic
import           Network.Xmpp.Concurrent.Types
import           Network.Xmpp.Types

-- | Sends an IQ, returns Just a 'TMVar' that will be filled with the first
-- inbound IQ with a matching ID that has type @result@ or @error@ or Nothing if
-- the stanza could not be sent
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
       -> IO (Maybe (TMVar (Maybe IQResponse)))
sendIQ timeOut to tp lang body session = do -- TODO: Add timeout
    newId <- idGenerator session
    ref <- atomically $ do
        resRef <- newEmptyTMVar
        (byNS, byId) <- readTVar (iqHandlers session)
        writeTVar (iqHandlers session) (byNS, Map.insert newId resRef byId)
          -- TODO: Check for id collisions (shouldn't happen?)
        return resRef
    res <- sendStanza (IQRequestS $ IQRequest newId Nothing to lang tp body) session
    if res
        then do
            case timeOut of
                Nothing -> return ()
                Just t -> void . forkIO $ do
                          delay t
                          doTimeOut (iqHandlers session) newId ref
            return $ Just ref
        else return Nothing
  where
    doTimeOut handlers iqid var = atomically $ do
      p <- tryPutTMVar var IQResponseTimeout
      when p $ do
          (byNS, byId) <- readTVar (iqHandlers session)
          writeTVar handlers (byNS, Map.delete iqid byId)
      return ()

-- | Like 'sendIQ', but waits for the answer IQ.
sendIQ' :: Maybe Integer
        -> Maybe Jid
        -> IQRequestType
        -> Maybe LangTag
        -> Element
        -> Session
        -> IO (Maybe IQResponse)
sendIQ' timeout to tp lang body session = do
    ref <- sendIQ timeout to tp lang body session
    maybe (return Nothing) (fmap Just . atomically . takeTMVar) ref


-- | Retrieves an IQ listener channel. If the namespace/'IQRequestType' is not
-- already handled, a new 'TChan' is created and returned as a 'Right' value.
-- Otherwise, the already existing channel will be returned wrapped in a 'Left'
-- value. The 'Left' channel might need to be duplicated in order not
-- to interfere with existing consumers.
--
-- Note thet every 'IQRequest' must be answered exactly once. To insure this,
-- the incoming requests are wrapped in an 'IQRequestTicket' that prevents
-- multiple responses. Use 'iqRequestBody' to extract the corresponding request
-- and 'answerIQ' to send the response
listenIQChan :: IQRequestType  -- ^ Type of IQs to receive ('Get' or 'Set')
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

-- | Answer an IQ request. Only the first answer ist sent and Just True is
-- returned when the answer is sucessfully sent. If an error occured during
-- sending Just False is returned (and another attempt can be
-- undertaken). Subsequent answers after the first sucessful one are dropped and
-- (False is returned in that case)
answerIQ :: IQRequestTicket
         -> Either StanzaError (Maybe Element)
         -> IO (Maybe Bool)
answerIQ ticket = answerTicket ticket
