{-# OPTIONS_HADDOCK hide #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Network.Xmpp.Concurrent.Types where

import qualified Control.Exception.Lifted as Ex
import           Control.Concurrent
import           Control.Concurrent.STM

import qualified Data.ByteString as BS
import           Data.IORef
import qualified Data.Map as Map
import           Data.Text(Text)
import           Data.Typeable

import           Network.Xmpp.Types

-- | Handlers to be run when the Xmpp session ends and when the Xmpp connection is
-- closed.
data EventHandlers = EventHandlers
    { connectionClosedHandler :: StreamError -> IO ()
    }

-- | Xmpp Session object
data Session = Session
    { writeRef :: TMVar (BS.ByteString -> IO Bool)
    , readerThread :: ThreadId
    , idGenerator :: IO StanzaId
      -- | Lock (used by withConnection) to make sure that a maximum of one
      -- XmppConMonad action is executed at any given time.
    , conStateRef :: TMVar XmppConnection
    , eventHandlers :: TVar EventHandlers
    , stopThreads :: IO ()
    }


-- | Interrupt is used to signal to the reader thread that it should stop. Th contained semphore signals the reader to resume it's work.
data Interrupt = Interrupt (TMVar ()) deriving Typeable
instance Show Interrupt where show _ = "<Interrupt>"

instance Ex.Exception Interrupt
