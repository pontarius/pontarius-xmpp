{-# OPTIONS_HADDOCK hide #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Network.Xmpp.Concurrent.Types where

import           Control.Concurrent
import           Control.Concurrent.STM
import qualified Control.Exception.Lifted as Ex
import qualified Data.ByteString as BS
import qualified Data.Map as Map
import           Data.Text (Text)
import           Data.Typeable

import           Network.Xmpp.IM.Roster.Types
import           Network.Xmpp.Types

-- | Handlers to be run when the Xmpp session ends and when the Xmpp connection is
-- closed.
data EventHandlers = EventHandlers
    { connectionClosedHandler :: XmppFailure -> IO ()
    }

-- | Interrupt is used to signal to the reader thread that it should stop. Th contained semphore signals the reader to resume it's work.
data Interrupt = Interrupt (TMVar ()) deriving Typeable
instance Show Interrupt where show _ = "<Interrupt>"

instance Ex.Exception Interrupt


-- | A concurrent interface to Pontarius XMPP.
data Session = Session
    { stanzaCh :: TChan Stanza -- All stanzas
    , outCh :: TChan Stanza
    , iqHandlers :: TVar IQHandlers
      -- Writing lock, so that only one thread could write to the stream at any
      -- given time.
      -- Fields below are from Context.
    , writeRef :: TMVar (BS.ByteString -> IO Bool)
    , readerThread :: ThreadId
    , idGenerator :: IO StanzaID
      -- | Lock (used by withStream) to make sure that a maximum of one
      -- Stream action is executed at any given time.
    , streamRef :: TMVar (Stream)
    , eventHandlers :: TVar EventHandlers
    , stopThreads :: IO ()
    , rosterRef :: TVar Roster
    , conf :: SessionConfiguration
    }

-- | IQHandlers holds the registered channels for incomming IQ requests and
-- TMVars of and TMVars for expected IQ responses
type IQHandlers = (Map.Map (IQRequestType, Text) (TChan IQRequestTicket)
                  , Map.Map StanzaID (TMVar IQResponse)
                  )

-- | Contains whether or not a reply has been sent, and the IQ request body to
-- reply to.
data IQRequestTicket = IQRequestTicket
    { sentRef     :: (TVar Bool)
    , iqRequestBody :: IQRequest
    }
