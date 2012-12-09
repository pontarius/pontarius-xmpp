{-# OPTIONS_HADDOCK hide #-}
module Network.Xmpp.Concurrent.Channels.Types where

import           Control.Concurrent.STM
import           Data.IORef
import qualified Data.Map as Map
import           Data.Text (Text)
import           Network.Xmpp.Concurrent.Types
import           Network.Xmpp.Types

-- | An XMPP session context
data Session = Session
    { context :: Context
    , stanzaCh :: TChan Stanza -- All stanzas
    , outCh :: TChan Stanza
    , iqHandlers :: TVar IQHandlers
      -- Writing lock, so that only one thread could write to the stream at any
      -- given time.
    }

-- | IQHandlers holds the registered channels for incomming IQ requests and
-- TMVars of and TMVars for expected IQ responses
type IQHandlers = (Map.Map (IQRequestType, Text) (TChan IQRequestTicket)
                  , Map.Map StanzaId (TMVar IQResponse)
                  )

-- | Contains whether or not a reply has been sent, and the IQ request body to
-- reply to.
data IQRequestTicket = IQRequestTicket
    { sentRef     :: (TVar Bool)
    , iqRequestBody :: IQRequest
    }
