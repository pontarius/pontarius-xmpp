{-# OPTIONS_HADDOCK hide #-}
module Network.Xmpp.Concurrent.Basic where

import Control.Concurrent.STM
import Network.Xmpp.Concurrent.Types
import Network.Xmpp.Stream
import Network.Xmpp.Types
import Control.Monad.State.Strict

-- | Send a stanza to the server.
sendStanza :: Stanza -> Session -> IO ()
sendStanza a session = atomically $ writeTChan (outCh session) a

-- | Get the channel of incoming stanzas.
getStanzaChan :: Session -> TChan Stanza
getStanzaChan session = stanzaCh session

-- | Get the next incoming stanza
getStanza :: Session -> IO Stanza
getStanza session = atomically . readTChan $ stanzaCh session

-- | Create a new session object with the inbound channel duplicated
dupSession :: Session -> IO Session
dupSession session = do
    stanzaCh' <- atomically $ dupTChan (stanzaCh session)
    return $ session {stanzaCh = stanzaCh'}

-- | Return the JID assigned to us by the server
getJid :: Session -> IO (Maybe Jid)
getJid Session{streamRef = st} = do
    s <- atomically $ readTMVar st
    withStream' (gets streamJid) s
