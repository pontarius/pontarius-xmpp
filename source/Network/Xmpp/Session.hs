{-# OPTIONS_HADDOCK hide #-}
{-# LANGUAGE OverloadedStrings #-}
module Network.Xmpp.Session where

import qualified Control.Exception as Ex
import           Control.Monad.Error
import           Data.Text as Text
import           Data.XML.Pickle
import           Data.XML.Types(Element)
import           Network
import qualified Network.TLS as TLS
import           Network.Xmpp.Bind
import           Network.Xmpp.Concurrent.Types
import           Network.Xmpp.Concurrent
import           Network.Xmpp.Connection_
import           Network.Xmpp.Marshal
import           Network.Xmpp.Pickle
import           Network.Xmpp.Sasl
import           Network.Xmpp.Sasl.Mechanisms
import           Network.Xmpp.Sasl.Types
import           Network.Xmpp.Stream
import           Network.Xmpp.Tls
import           Network.Xmpp.Types
import           Control.Concurrent.STM.TMVar
import           Data.Maybe

-- | Creates a 'Session' object by setting up a connection with an XMPP server.
-- 
-- Will connect to the specified host. If the fourth parameters is a 'Just'
-- value, @session@ will attempt to secure the connection with TLS. If the fifth
-- parameters is a 'Just' value, @session@ will attempt to authenticate and
-- acquire an XMPP resource.
session :: HostName                          -- ^ Host to connect to
        -> Text                              -- ^ The realm host name (to
                                             -- distinguish the XMPP service)
        -> PortID                            -- ^ Port to connect to
        -> Maybe TLS.TLSParams               -- ^ TLS settings, if securing the
                                             -- connection to the server is
                                             -- desired
        -> Maybe ([SaslHandler], Maybe Text) -- ^ SASL handlers and the desired
                                             -- JID resource (or Nothing to let
                                             -- the server decide)
        -> IO (Either XmppFailure (Session, Maybe AuthFailure))
session hostname realm port tls sasl = runErrorT $ do
    con <- ErrorT $ connect hostname port realm
    if isJust tls
        then ErrorT $ startTls (fromJust tls) con
        else return ()
    aut <- if isJust sasl
               then ErrorT $ auth (fst $ fromJust sasl) (snd $ fromJust sasl) con
               else return Nothing
    ses <- ErrorT $ newSession con
    return (ses, aut)
    
-- | Connects to the XMPP server and opens the XMPP stream against the given
-- host name, port, and realm.
connect :: HostName -> PortID -> Text -> IO (Either XmppFailure (TMVar Connection))
connect address port hostname = do
    con <- connectTcp address port hostname
    case con of
        Right con' -> do
            result <- withConnection startStream con'
            return $ Right con'
        Left e -> do
            return $ Left e

sessionXml :: Element
sessionXml = pickleElem
    (xpElemBlank "{urn:ietf:params:xml:ns:xmpp-session}session")
    ()

sessionIQ :: Stanza
sessionIQ = IQRequestS $ IQRequest { iqRequestID      = "sess"
                                   , iqRequestFrom    = Nothing
                                   , iqRequestTo      = Nothing
                                   , iqRequestLangTag = Nothing
                                   , iqRequestType    = Set
                                   , iqRequestPayload = sessionXml
                                   }

-- Sends the session IQ set element and waits for an answer. Throws an error if
-- if an IQ error stanza is returned from the server.
startSession :: TMVar Connection -> IO ()
startSession con = do
    answer <- pushIQ' "session" Nothing Set Nothing sessionXml con
    case answer of
        Left e -> error $ show e
        Right _ -> return ()

-- | Authenticate to the server using the first matching method and bind a
-- resource.
auth :: [SaslHandler]
     -> Maybe Text
     -> TMVar Connection
     -> IO (Either XmppFailure (Maybe AuthFailure))
auth mechanisms resource con = runErrorT $ do
    ErrorT $ xmppSasl mechanisms con
    jid <- lift $ xmppBind resource con
    lift $ startSession con
    return Nothing

-- | Authenticate to the server with the given username and password
-- and bind a resource.
--
-- Prefers SCRAM-SHA1 over DIGEST-MD5.
simpleAuth  :: Text.Text  -- ^ The username
            -> Text.Text  -- ^ The password
            -> Maybe Text -- ^ The desired resource or 'Nothing' to let the
                          -- server assign one
            -> TMVar Connection
            -> IO (Either XmppFailure (Maybe AuthFailure))
simpleAuth username passwd resource = flip auth resource $
        [ -- TODO: scramSha1Plus
          scramSha1 username Nothing passwd
        , digestMd5 username Nothing passwd
        ]
