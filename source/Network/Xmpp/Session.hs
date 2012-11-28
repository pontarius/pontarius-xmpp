{-# OPTIONS_HADDOCK hide #-}
{-# LANGUAGE OverloadedStrings #-}
module Network.Xmpp.Session where

import           Control.Monad.Error
import           Data.Text as Text
import           Data.XML.Pickle
import           Data.XML.Types(Element)
import qualified Network.TLS as TLS
import           Network.Xmpp.Bind
import           Network.Xmpp.Concurrent.Types
import           Network.Xmpp.Marshal
import           Network.Xmpp.Monad
import           Network.Xmpp.Pickle
import           Network.Xmpp.Sasl
import           Network.Xmpp.Sasl.Mechanisms
import           Network.Xmpp.Sasl.Types
import           Network.Xmpp.Stream
import           Network.Xmpp.Types
import           Network
import           Network.Xmpp.TLS

-- | The quick and easy way to set up a connection to an XMPP server
--
-- This will
--
--   * connect to the host
--
--   * secure the connection with TLS
--
--   * authenticate to the server using either SCRAM-SHA1 (preferred) or
--     Digest-MD5
--
--   * bind a resource
--
--   * return the full JID you have been assigned
--
-- Note that the server might assign a different resource even when we send
-- a preference.
simpleConnect :: HostName   -- ^ Host to connect to
              -> PortID     -- ^ Port to connec to
              -> Text       -- ^ Hostname of the server (to distinguish the XMPP
                            -- service)
              -> Text       -- ^ User name (authcid)
              -> Text       -- ^ Password
              -> Maybe Text -- ^ Desired resource (or Nothing to let the server
                            -- decide)
              -> XmppConMonad Jid
simpleConnect host port hostname username password resource = do
      connect host port hostname
      startTLS exampleParams
      saslResponse <- simpleAuth username password resource
      case saslResponse of
          Right jid -> return jid
          Left e -> error $ show e


-- | Connect to host with given address.
connect :: HostName -> PortID -> Text -> XmppConMonad (Either StreamError ())
connect address port hostname = do
    xmppRawConnect address port hostname
    result <- xmppStartStream
    case result of
        Left e -> do
            pushElement . pickleElem xpStreamError $ toError e
            xmppCloseStreams
            return ()
        Right () -> return ()
    return result
  where
        -- TODO: Descriptive texts in stream errors?
        toError  (StreamNotStreamElement _name) =
                XmppStreamError StreamInvalidXml Nothing Nothing
        toError  (StreamInvalidStreamNamespace _ns) =
                XmppStreamError StreamInvalidNamespace Nothing Nothing
        toError  (StreamInvalidStreamPrefix _prefix) =
                XmppStreamError StreamBadNamespacePrefix Nothing Nothing
        -- TODO: Catch remaining xmppStartStream errors.
        toError  (StreamWrongVersion _ver) =
                XmppStreamError StreamUnsupportedVersion Nothing Nothing
        toError  (StreamWrongLangTag _) =
                XmppStreamError StreamInvalidXml Nothing Nothing
        toError  StreamUnknownError =
                XmppStreamError StreamBadFormat Nothing Nothing


sessionXML :: Element
sessionXML = pickleElem
    (xpElemBlank "{urn:ietf:params:xml:ns:xmpp-session}session")
    ()

sessionIQ :: Stanza
sessionIQ = IQRequestS $ IQRequest { iqRequestID      = "sess"
                                   , iqRequestFrom    = Nothing
                                   , iqRequestTo      = Nothing
                                   , iqRequestLangTag = Nothing
                                   , iqRequestType    = Set
                                   , iqRequestPayload = sessionXML
                                   }

-- Sends the session IQ set element and waits for an answer. Throws an error if
-- if an IQ error stanza is returned from the server.
xmppStartSession :: XmppConMonad ()
xmppStartSession = do
    answer <- xmppSendIQ' "session" Nothing Set Nothing sessionXML
    case answer of
        Left e -> error $ show e
        Right _ -> return ()

-- | Authenticate to the server using the first matching method and bind a
-- resource.
auth :: [SaslHandler]
     -> Maybe Text
     -> XmppConMonad (Either AuthError Jid)
auth mechanisms resource = runErrorT $ do
    ErrorT $ xmppSasl mechanisms
    jid <- lift $ xmppBind resource
    lift $ xmppStartSession
    return jid

-- | Authenticate to the server with the given username and password
-- and bind a resource.
--
-- Prefers SCRAM-SHA1 over DIGEST-MD5.
simpleAuth  :: Text.Text  -- ^ The username
            -> Text.Text  -- ^ The password
            -> Maybe Text -- ^ The desired resource or 'Nothing' to let the
                          -- server assign one
            -> XmppConMonad (Either AuthError Jid)
simpleAuth username passwd resource = flip auth resource $
        [ -- TODO: scramSha1Plus
          scramSha1 username Nothing passwd
        , digestMd5 username Nothing passwd
        ]
