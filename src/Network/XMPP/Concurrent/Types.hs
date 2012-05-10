{-# OPTIONS_HADDOCK hide #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Network.XMPP.Concurrent.Types where

import qualified Control.Exception.Lifted as Ex
import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Monad.Trans.Reader

import qualified Data.ByteString as BS
import           Data.IORef
import qualified Data.Map as Map
import           Data.Text(Text)
import           Data.Typeable

import           Network.XMPP.Types

-- Map between the IQ request type and the "query" namespace pair, and the TChan
-- for the IQ request and "sent" boolean pair.
type IQHandlers = ( Map.Map (IQRequestType, Text) (TChan (IQRequest, TVar Bool))
                  , Map.Map StanzaId (TMVar IQResponse)
                  )

-- Handlers to be run when the XMPP session ends and when the XMPP connection is
-- closed.
data EventHandlers = EventHandlers
    { sessionEndHandler       :: IO ()
    , connectionClosedHandler :: StreamError -> IO ()
    }

-- The Session object is the XMPP (ReaderT) state. 
data Session = Session
    { -- The original master channels that the reader puts stanzas into. These
      -- are cloned by @get{Message,Presence}Chan on demand when first used by
      -- the thread and are stored in the {message,presence}Ref fields below.
      mShadow :: TChan (Either MessageError Message)
    , pShadow :: TChan (Either PresenceError Presence)
      -- The cloned copies of the original/shadow channels. They are
      -- thread-local (as opposed to the shadow channels) and contains all
      -- stanzas received after the cloning of the shadow channels.
    , messagesRef :: IORef (Maybe (TChan (Either MessageError Message)))
    , presenceRef :: IORef (Maybe (TChan (Either PresenceError Presence)))
    , outCh :: TChan Stanza
    , iqHandlers :: TVar IQHandlers
      -- Writing lock, so that only one thread could write to the stream at any
      -- given time.
    , writeRef :: TMVar (BS.ByteString -> IO Bool)
    , readerThread :: ThreadId
    , idGenerator :: IO StanzaId
      -- Lock (used by withConnection) to make sure that a maximum of one
      -- XMPPConMonad calculation is executed at any given time.
    , conStateRef :: TMVar XmppConnection
    , eventHandlers :: TVar EventHandlers
    , stopThreads :: IO ()
    }

-- XMPP is a monad for concurrent XMPP usage.
type XMPP a = ReaderT Session IO a

-- Interrupt is used to signal to the reader thread that it should stop.
data Interrupt = Interrupt (TMVar ()) deriving Typeable
instance Show Interrupt where show _ = "<Interrupt>"
instance Ex.Exception Interrupt