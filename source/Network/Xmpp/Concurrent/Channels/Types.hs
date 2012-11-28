{-# OPTIONS_HADDOCK hide #-}
module Network.Xmpp.Concurrent.Channels.Types where

import           Control.Concurrent.STM
import           Data.IORef
import qualified Data.Map as Map
import           Data.Text (Text)
import           Network.Xmpp.Concurrent.Types
import           Network.Xmpp.Types

-- | An XMPP session context
data Context = Context
    { session :: Session
      -- The original master channels that the reader puts stanzas
      -- into. These are cloned by @get{STanza,Message,Presence}Chan
      -- on demand when first used by the thread and are stored in the
      -- {message,presence}Ref fields below.
    , mShadow :: TChan (Either MessageError Message)
    , pShadow :: TChan (Either PresenceError Presence)
    , sShadow :: TChan Stanza -- All stanzas
      -- The cloned copies of the original/shadow channels. They are
      -- thread-local (as opposed to the shadow channels) and contains all
      -- stanzas received after the cloning of the shadow channels.
    , messagesRef :: IORef (Maybe (TChan (Either MessageError Message)))
    , presenceRef :: IORef (Maybe (TChan (Either PresenceError Presence)))
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
