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
import           Network.Xmpp.Concurrent.Channels
import           Network.Xmpp.Connection
import           Network.Xmpp.Marshal
import           Network.Xmpp.Pickle
import           Network.Xmpp.Sasl
import           Network.Xmpp.Sasl.Mechanisms
import           Network.Xmpp.Sasl.Types
import           Network.Xmpp.Stream
import           Network.Xmpp.Tls
import           Network.Xmpp.Types

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
              -> IO Session
simpleConnect host port hostname username password resource = do
      con' <- connectTcp host port hostname
      con <- case con' of
          Left e -> Ex.throwIO e
          Right r -> return r
      startTls exampleParams con
      saslResponse <- simpleAuth username password resource con
      case saslResponse of
          Right jid -> newSession con
          Left e -> error $ show e


-- | Connect to host with given address.
connectTcp :: HostName -> PortID -> Text -> IO (Either StreamFailure Connection)
connectTcp address port hostname = do
    con <- connectTcpRaw address port hostname
    result <- withConnection startStream con
    case result of
        Left e -> do
            withConnection (pushElement . pickleElem xpStreamError $ toError e)
                           con
            closeStreams con
            return $ Left e
        Right () -> return $ Right con
  where
        -- toError  (StreamNotStreamElement _name) =
        --         XmppStreamFailure StreamInvalidXml Nothing Nothing
        -- toError  (StreamInvalidStreamNamespace _ns) =
        --         XmppStreamFailure StreamInvalidNamespace Nothing Nothing
        -- toError  (StreamInvalidStreamPrefix _prefix) =
        --         XmppStreamFailure StreamBadNamespacePrefix Nothing Nothing
        -- toError  (StreamWrongVersion _ver) =
        --         XmppStreamFailure StreamUnsupportedVersion Nothing Nothing
        -- toError  (StreamWrongLangTag _) =
        --         XmppStreamFailure StreamInvalidXml Nothing Nothing
        -- toError  StreamUnknownError =
        --         XmppStreamFailure StreamBadFormat Nothing Nothing
        -- TODO: Catch remaining xmppStartStream errors.
        toError _ = StreamErrorInfo StreamBadFormat Nothing Nothing

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
startSession :: Connection -> IO ()
startSession con = do
    answer <- pushIQ' "session" Nothing Set Nothing sessionXml con
    case answer of
        Left e -> error $ show e
        Right _ -> return ()

-- | Authenticate to the server using the first matching method and bind a
-- resource.
auth :: [SaslHandler]
     -> Maybe Text
     -> Connection
     -> IO (Either AuthError Jid)
auth mechanisms resource con = runErrorT $ do
    ErrorT $ xmppSasl mechanisms con
    jid <- lift $ xmppBind resource con
    lift $ startSession con
    return jid

-- | Authenticate to the server with the given username and password
-- and bind a resource.
--
-- Prefers SCRAM-SHA1 over DIGEST-MD5.
simpleAuth  :: Text.Text  -- ^ The username
            -> Text.Text  -- ^ The password
            -> Maybe Text -- ^ The desired resource or 'Nothing' to let the
                          -- server assign one
            -> Connection
            -> IO (Either AuthError Jid)
simpleAuth username passwd resource = flip auth resource $
        [ -- TODO: scramSha1Plus
          scramSha1 username Nothing passwd
        , digestMd5 username Nothing passwd
        ]
