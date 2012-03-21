{-# LANGUAGE OverloadedStrings #-}

module Network.XMPP.Session where

import Control.Monad.Trans.State

import Data.Text as Text

import Network.XMPP.Monad
import Network.XMPP.Types
import Network.XMPP.Pickle
import Network.XMPP.Marshal

import Text.XML.Expat.Pickle


sessionIQ :: Stanza
sessionIQ = SIQ $ IQ Nothing Nothing "sess" Set
                    (pickleElem
                      (xpElemNs "session"
                                "urn:ietf:params:xml:ns:xmpp-session"
                                xpUnit
                                xpUnit)
                      ((),())
                    )

xmppSession :: XMPPMonad ()
xmppSession = do
  push $ sessionIQ
  answer <- pull
  let SIQ (IQ Nothing Nothing "sess" Result b) = answer
  return ()