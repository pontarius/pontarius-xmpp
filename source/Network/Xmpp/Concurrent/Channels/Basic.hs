{-# OPTIONS_HADDOCK hide #-}
module Network.Xmpp.Concurrent.Channels.Basic where

import Control.Concurrent.STM
import Network.Xmpp.Concurrent.Channels.Types
import Network.Xmpp.Types

-- | Send a stanza to the server.
sendStanza :: Stanza -> Session -> IO ()
sendStanza a session = atomically $ writeTChan (outCh session) a

-- | Create a new session object with the inbound channel duplicated
dupSession :: Session -> IO Session
dupSession session = do
    stanzaCh' <- atomically $ dupTChan (stanzaCh session)
    return $ session {stanzaCh = stanzaCh'}
