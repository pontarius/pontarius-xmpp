{-# LANGUAGE OverloadedStrings #-}

module Network.XMPP.Session where

import Data.XML.Pickle

import Network.XMPP.Monad
import Network.XMPP.Pickle
import Network.XMPP.Types

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
  let SIQ (IQ Nothing Nothing "sess" Result _body) = answer
  return ()