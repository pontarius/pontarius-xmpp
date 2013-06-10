{-# OPTIONS_HADDOCK hide #-}
module Network.Xmpp.Concurrent.Basic where

import           Control.Concurrent.STM
import qualified Control.Exception as Ex
import           Control.Monad.State.Strict
import qualified Data.ByteString as BS
import           Network.Xmpp.Concurrent.Types
import           Network.Xmpp.Marshal
import           Network.Xmpp.Stream
import           Network.Xmpp.Types
import           Network.Xmpp.Utilities

semWrite :: WriteSemaphore -> BS.ByteString -> IO Bool
semWrite sem bs = Ex.bracket (atomically $ takeTMVar sem)
                          (atomically . putTMVar sem)
                          ($ bs)

writeStanza :: WriteSemaphore -> Stanza -> IO Bool
writeStanza sem a = do
    let outData = renderElement $ nsHack (pickleElem xpStanza a)
    semWrite sem outData

-- | Send a stanza to the server.
sendStanza :: Stanza -> Session -> IO Bool
sendStanza a session = writeStanza (writeSemaphore session) a


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

-- | Return the JID assigned to us by the server
getFeatures :: Session -> IO StreamFeatures
getFeatures Session{streamRef = st} = do
    s <- atomically $ readTMVar st
    withStream' (gets streamFeatures) s
