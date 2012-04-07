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
          -> IQType  -- ^ IQ type (Get or Set)
          -> Element -- ^ The iq body (there has to be exactly one)
          -> XMPPThread (TMVar IQ)
sendIQ to tp body = do -- TODO: add timeout
  newId <- liftIO =<< asks idGenerator
  handlers <- asks iqHandlers
  ref <- liftIO . atomically $ do
      resRef <- newEmptyTMVar
      (byNS, byId) <- readTVar handlers
      writeTVar handlers (byNS, Map.insert newId resRef byId)
        -- TODO: Check for id collisions (shouldn't happen?)
      return resRef
  sendS . SIQ $ IQ Nothing (to) newId tp body
  return ref

-- | like 'sendIQ', but waits for the answer IQ
sendIQ' :: Maybe JID -> IQType -> Element -> XMPPThread IQ
sendIQ' to tp body = do
  ref <- sendIQ to tp body
  liftIO . atomically $ takeTMVar ref

answerIQ :: MonadIO m => (IQ, TVar Bool) -> Element -> ReaderT Thread m Bool
answerIQ ((IQ from _to iqid _tp _bd), sentRef) body = do
  out <- asks outCh
  liftIO . atomically $ do
       sent <- readTVar sentRef
       case sent of
         False -> do
             writeTVar sentRef True
             writeTChan out . SIQ $ IQ Nothing from iqid Result body
             return True
         True -> return False
