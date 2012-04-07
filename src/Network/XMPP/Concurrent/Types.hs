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


type IQHandlers = (Map.Map (IQType, Text) (TChan (IQ, TVar Bool))
                  , Map.Map Text (TMVar IQ)
                  )

data Thread = Thread { messagesRef :: IORef (Maybe (TChan Message))
                     , presenceRef :: IORef (Maybe (TChan Presence))
                     , mShadow :: TChan Message -- the original chan
                     , pShadow :: TChan Presence -- the original chan
                     , outCh :: TChan Stanza
                     , iqHandlers :: TVar IQHandlers
                     , writeRef :: TMVar (BS.ByteString -> IO () )
                     , readerThread :: ThreadId
                     , idGenerator :: IO Text
                     }

type XMPPThread a = ReaderT Thread IO a


data ReaderSignal = ReaderSignal (XMPPMonad ()) deriving Typeable
instance Show ReaderSignal where show _ = "<ReaderSignal>"
instance Ex.Exception ReaderSignal
