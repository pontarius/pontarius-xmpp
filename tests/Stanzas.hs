{-# LANGUAGE OverloadedStrings #-}
module Tests.Stanzas where

import Data.Either
import Data.XML.Types
import Network.XMPP
import Network.XMPP.Marshal
import Network.XMPP.Pickle
import Network.XMPP.Types

xml1 = Element {elementName = Name {nameLocalName = "iq", nameNamespace = Just "jabber:client", namePrefix = Nothing}, elementAttributes = [(Name {nameLocalName = "id", nameNamespace = Nothing, namePrefix = Nothing},[ContentText "2"]), (Name {nameLocalName = "type", nameNamespace = Nothing, namePrefix = Nothing},[ContentText "error"]),(Name {nameLocalName = "to", nameNamespace = Nothing, namePrefix = Nothing},[ContentText "testuser1@species64739.dyndns.org/bot1"]),(Name {nameLocalName = "from", nameNamespace = Nothing, namePrefix = Nothing},[ContentText "testuser2@species64739.dyndns.org/bot2"])], elementNodes = [NodeElement (Element {elementName = Name {nameLocalName = "error", nameNamespace = Just "jabber:client", namePrefix = Nothing}, elementAttributes = [(Name {nameLocalName = "type", nameNamespace = Nothing, namePrefix = Nothing},[ContentText "cancel"])], elementNodes = [NodeElement (Element {elementName = Name {nameLocalName = "service-unavailable", nameNamespace = Just "urn:ietf:params:xml:ns:xmpp-stanzas", namePrefix = Nothing}, elementAttributes = [], elementNodes = []})]})]}

isRight (Right _) = True
isRight  _ = False


testXML1 = isRight $ unpickleElem stanzaP xml1