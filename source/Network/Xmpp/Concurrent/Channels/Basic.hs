{-# OPTIONS_HADDOCK hide #-}
module Network.Xmpp.Concurrent.Channels.Basic where

import Control.Concurrent.STM
import Data.IORef
import Network.Xmpp.Concurrent.Channels.Types
import Network.Xmpp.Types

-- | Get a duplicate of the stanza channel
getStanzaChan :: Session -> IO (TChan Stanza)
getStanzaChan session = atomically $ dupTChan (sShadow session)

-- | Send a stanza to the server.
sendStanza :: Stanza -> Session -> IO ()
sendStanza a session = atomically $ writeTChan (outCh session) a

-- | Create a forked session object
forkSession :: Session -> IO Session
forkSession session = do
    mCH' <- newIORef Nothing
    pCH' <- newIORef Nothing
    return $ session {messagesRef = mCH' , presenceRef = pCH'}
