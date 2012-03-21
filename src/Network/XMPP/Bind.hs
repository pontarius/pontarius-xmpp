{-# LANGUAGE OverloadedStrings #-}

module Network.XMPP.Bind where

import Control.Monad.Trans.State

import Data.Text as Text

import Network.XMPP.Monad
import Network.XMPP.Types
import Network.XMPP.Pickle
import Network.XMPP.Marshal

import Text.XML.Expat.Pickle

bindReqIQ :: Maybe Text -> Stanza
bindReqIQ resource= SIQ $ IQ Nothing Nothing "bind" Set
                    (pickleElem
                      (bindP . xpOption
                         $ xpElemNodes "resource" (xpContent xpText))
                      resource
                    )

jidP :: PU [Node Text Text] JID
jidP = bindP $ xpElemNodes "jid" (xpContent xpPrim)

xmppBind :: XMPPMonad ()
xmppBind = do
  res <- gets sResource
  push $ bindReqIQ res
  answer <- pull
  let SIQ (IQ Nothing Nothing _ Result b) = answer
  let (JID n d (Just r)) = unpickleElem jidP b
  modify (\s -> s{sResource = Just r})

bindP  :: PU [Node Text.Text Text.Text] b -> PU [Node Text.Text Text.Text] b
bindP c = ignoreAttrs $ xpElemNs "bind" "urn:ietf:params:xml:ns:xmpp-bind"
                          xpUnit
                          c


