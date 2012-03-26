{-# LANGUAGE OverloadedStrings #-}

module Network.XMPP.Session where

import Control.Monad.Trans.State

import Data.Text as Text

import Data.XML.Pickle
import Data.XML.Types

import Network.XMPP.Monad
import Network.XMPP.Types
import Network.XMPP.Pickle
import Network.XMPP.Marshal


sessionIQ :: Stanza
sessionIQ = SIQ $ IQ Nothing Nothing "sess" Set
                    (pickleElem
                      (xpElemBlank "{urn:ietf:params:xml:ns:xmpp-session}session")
                      ()
                    )

xmppSession :: XMPPMonad ()
xmppSession = do
  push $ sessionIQ
  answer <- pull
  let SIQ (IQ Nothing Nothing "sess" Result b) = answer
  return ()