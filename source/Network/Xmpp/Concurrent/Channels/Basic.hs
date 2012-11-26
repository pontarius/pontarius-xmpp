module Network.Xmpp.Concurrent.Channels.Basic where

import Control.Concurrent.STM
import Data.IORef
import Network.Xmpp.Concurrent.Channels.Types
import Network.Xmpp.Types

-- | Get a duplicate of the stanza channel
getStanzaChan :: CSession -> IO (TChan Stanza)
getStanzaChan session = atomically $ dupTChan (sShadow session)

-- | Send a stanza to the server.
sendStanza :: Stanza -> CSession -> IO ()
sendStanza a session = atomically $ writeTChan (outCh session) a

-- | Create a forked session object
forkCSession :: CSession -> IO CSession
forkCSession session = do
    mCH' <- newIORef Nothing
    pCH' <- newIORef Nothing
    return $ session {messagesRef = mCH' , presenceRef = pCH'}
