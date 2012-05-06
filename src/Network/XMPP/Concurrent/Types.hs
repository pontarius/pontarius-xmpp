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


type IQHandlers = (Map.Map (IQRequestType, Text) (TChan (IQRequest, TVar Bool))
                  , Map.Map StanzaId (TMVar IQResponse)
                  )

data EventHandlers = EventHandlers
                         { sessionEndHandler       :: IO ()
                         , connectionClosedHandler :: StreamError -> IO ()
                         }

zeroEventHandlers :: EventHandlers
zeroEventHandlers = EventHandlers
                         { sessionEndHandler       = return ()
                         , connectionClosedHandler = \_ -> return ()
                         }

data Session = Session { messagesRef :: IORef (Maybe ( TChan (Either
                                                              MessageError
                                                              Message
                                                             )))
                       , presenceRef :: IORef (Maybe (TChan (Either
                                            PresenceError Presence )))
                       , mShadow :: TChan (Either MessageError
                                                  Message)
                                          -- the original chan
                       , pShadow :: TChan (Either PresenceError
                                                  Presence)
                                           -- the original chan
                       , outCh :: TChan Stanza
                       , iqHandlers :: TVar IQHandlers
                       , writeRef :: TMVar (BS.ByteString -> IO Bool )
                       , readerThread :: ThreadId
                       , idGenerator :: IO StanzaId
                       , conStateRef :: TMVar XmppConnection
                       , eventHandlers :: TVar EventHandlers
                       , stopThreads :: IO ()
                       }

type XMPP a = ReaderT Session IO a

data Interrupt = Interrupt (TMVar ()) deriving Typeable
instance Show Interrupt where show _ = "<Interrupt>"
instance Ex.Exception Interrupt
