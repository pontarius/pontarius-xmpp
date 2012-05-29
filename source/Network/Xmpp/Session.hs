{-# LANGUAGE OverloadedStrings #-}

module Network.Xmpp.Session where

import Data.XML.Pickle
import Data.XML.Types(Element)

import Network.Xmpp.Monad
import Network.Xmpp.Pickle
import Network.Xmpp.Types
import Network.Xmpp.Concurrent

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

-- Sends the session IQ set element and waits for an answer. Throws an error if
-- if an IQ error stanza is returned from the server.
startSession :: Xmpp ()
startSession = do
    answer <- sendIQ' Nothing Set Nothing sessionXML
    case answer of
        Left e -> error $ show e
        Right _ -> return ()
