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
                         { sessionEndHandler       :: XMPPThread ()
                         , connectionClosedHandler :: XMPPThread ()
                         }

zeroEventHandlers = EventHandlers
                         { sessionEndHandler       = return ()
                         , connectionClosedHandler = return ()
                         }

data Thread = Thread { messagesRef :: IORef (Maybe ( TChan (Either
                                                              MessageError
                                                              Message
                                                           )))
                     , presenceRef :: IORef (Maybe (TChan (Either
                                                              PresenceError
                                                              Presence
                                                          )))
                     , mShadow :: TChan (Either MessageError
                                                Message) -- the original chan
                     , pShadow :: TChan (Either PresenceError
                                                Presence) -- the original chan
                     , outCh :: TChan Stanza
                     , iqHandlers :: TVar IQHandlers
                     , writeRef :: TMVar (BS.ByteString -> IO () )
                     , readerThread :: ThreadId
                     , idGenerator :: IO StanzaId
                     , conStateRef :: TMVar XMPPConState
                     , eventHandlers :: TVar EventHandlers
                     , stopThreads :: IO ()
                     }

type XMPPThread a = ReaderT Thread IO a

data Interrupt = Interrupt (TMVar ()) deriving Typeable
instance Show Interrupt where show _ = "<Interrupt>"
instance Ex.Exception Interrupt
