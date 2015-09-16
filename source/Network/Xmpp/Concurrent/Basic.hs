{-# OPTIONS_HADDOCK hide #-}
module Network.Xmpp.Concurrent.Basic where

import           Control.Applicative
import           Control.Concurrent.STM
import qualified Control.Exception as Ex
import           Control.Monad.State.Strict
import qualified Data.ByteString as BS
import           Network.Xmpp.Concurrent.Types
import           Network.Xmpp.Marshal
import           Network.Xmpp.Stream
import           Network.Xmpp.Types
import           Network.Xmpp.Utilities

semWrite :: WriteSemaphore -> BS.ByteString -> IO (Either XmppFailure ())
semWrite sem bs = Ex.bracket (atomically $ takeTMVar sem)
                          (atomically . putTMVar sem)
                          ($ bs)

writeStanza :: WriteSemaphore -> Stanza -> IO (Either XmppFailure ())
writeStanza sem a = do
    let outData = renderElement $ nsHack (pickleElem xpStanza a)
    debugOut outData
    semWrite sem outData


-- | Send a stanza to the server without running plugins. (The stanza is sent as
-- is)
sendRawStanza :: Stanza -> Session -> IO (Either XmppFailure ())
sendRawStanza a session = writeStanza (writeSemaphore session) a

-- | Send a stanza to the server, managed by plugins
sendStanza :: Stanza -> Session -> IO (Either XmppFailure ())
sendStanza = flip sendStanza'

-- | Get the channel of incoming stanzas.
getStanzaChan :: Session -> TChan (Stanza, [Annotation])
getStanzaChan session = stanzaCh session

-- | Get the next incoming stanza
getStanza :: Session -> IO (Stanza, [Annotation])
getStanza session = atomically . readTChan $ stanzaCh session

-- | Duplicate the inbound channel of the session object. Most receiving
-- functions discard stanzas they are not interested in from the inbound
-- channel. Duplicating the channel ensures that those stanzas can aren't lost
-- and can still be handled somewhere else.
dupSession :: Session -> IO Session
dupSession session = do
    stanzaCh' <- atomically $ cloneTChan (stanzaCh session)
    return $ session {stanzaCh = stanzaCh'}

-- | Return the JID assigned to us by the server
getJid :: Session -> IO (Maybe Jid)
getJid Session{streamRef = st} = do
    s <- atomically $ readTMVar st
    withStream' (gets streamJid) s

-- | Return the stream features the server announced
getFeatures :: Session -> IO StreamFeatures
getFeatures Session{streamRef = st} = do
    s <- atomically $ readTMVar st
    withStream' (gets streamFeatures) s

-- | Wait until the connection of the stream is re-established
waitForStream :: Session -> IO ()
waitForStream Session{streamRef = sr} = atomically $ do
    s <- readTMVar sr
    ss <- readTMVar $ unStream s
    case streamConnectionState ss of
        Plain -> return ()
        Secured -> return ()
        _ -> retry

streamState :: Session -> STM ConnectionState
streamState Session{streamRef = sr}  = do
    s <- readTMVar sr
    streamConnectionState <$> (readTMVar $ unStream s)
