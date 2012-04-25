module Network.XMPP.Concurrent.IQ where

import Control.Concurrent.STM
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader

import Data.XML.Types
import qualified Data.Map as Map

import Network.XMPP.Concurrent.Types
import Network.XMPP.Concurrent.Monad
import Network.XMPP.Types

-- | Sends an IQ, returns a 'TMVar' that will be filled with the first inbound
-- IQ with a matching ID that has type @result@ or @error@
sendIQ :: Maybe JID -- ^ Recipient (to)
          -> IQRequestType  -- ^ IQ type (Get or Set)
          -> Maybe LangTag  -- ^ Language tag of the payload (Nothing for default)
          -> Element -- ^ The iq body (there has to be exactly one)
          -> XMPP (TMVar IQResponse)
sendIQ to tp lang body = do -- TODO: add timeout
  newId <- liftIO =<< asks idGenerator
  handlers <- asks iqHandlers
  ref <- liftIO . atomically $ do
      resRef <- newEmptyTMVar
      (byNS, byId) <- readTVar handlers
      writeTVar handlers (byNS, Map.insert newId resRef byId)
        -- TODO: Check for id collisions (shouldn't happen?)
      return resRef
  sendS . IQRequestS $ IQRequest newId Nothing to lang tp body
  return ref

-- | like 'sendIQ', but waits for the answer IQ
sendIQ' :: Maybe JID
        -> IQRequestType
        -> Maybe LangTag
        -> Element
        -> XMPP IQResponse
sendIQ' to tp lang body = do
  ref <- sendIQ to tp lang body
  liftIO . atomically $ takeTMVar ref

answerIQ :: (IQRequest, TVar Bool)
         -> Either StanzaError (Maybe Element)
         -> XMPP Bool
answerIQ ((IQRequest iqid from _to lang _tp bd), sentRef) answer = do
  out <- asks outCh
  let response = case answer of
        Left err  -> IQErrorS $ IQError iqid Nothing from lang err (Just bd)
        Right res -> IQResultS $ IQResult iqid Nothing from lang res
  liftIO . atomically $ do
       sent <- readTVar sentRef
       case sent of
         False -> do
             writeTVar sentRef True

             writeTChan out response
             return True
         True -> return False
