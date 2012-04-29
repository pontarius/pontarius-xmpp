{-# LANGUAGE OverloadedStrings #-}

module Network.XMPP.Session where

import Data.XML.Pickle
import Data.XML.Types(Element)

import Network.XMPP.Monad
import Network.XMPP.Pickle
import Network.XMPP.Types
import Network.XMPP.Concurrent


sessionXML :: Element
sessionXML = pickleElem
                (xpElemBlank "{urn:ietf:params:xml:ns:xmpp-session}session" )
                ()

sessionIQ :: Stanza
sessionIQ = IQRequestS $ IQRequest { iqRequestID      = "sess"
                                   , iqRequestFrom    = Nothing
                                   , iqRequestTo      = Nothing
                                   , iqRequestLangTag = Nothing
                                   , iqRequestType    = Set
                                   , iqRequestPayload = sessionXML
                                   }

xmppStartSession :: XMPPConMonad ()
xmppStartSession = do
    answer <- xmppSendIQ' "session" Nothing Set Nothing sessionXML
    case answer of
        Left e -> error $ show e
        Right _ -> return ()


startSession :: XMPP ()
startSession = do
    answer <- sendIQ' Nothing Set Nothing sessionXML
    case answer of
        Left e -> error $ show e
        Right _ -> return ()
