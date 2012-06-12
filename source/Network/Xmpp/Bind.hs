{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_HADDOCK hide #-}

module Network.Xmpp.Bind where

import Control.Exception

import Data.Text as Text
import Data.XML.Pickle
import Data.XML.Types

import Network.Xmpp.Types
import Network.Xmpp.Pickle
import Network.Xmpp.Monad

import Control.Monad.State(modify)

-- Produces a `bind' element, optionally wrapping a resource.
bindBody :: Maybe Text -> Element
bindBody = pickleElem $
               -- Pickler to produce a
               -- "<bind xmlns='urn:ietf:params:xml:ns:xmpp-bind'/>"
               -- element, with a possible "<resource>[JID]</resource>"
               -- child.
               xpBind . xpOption $ xpElemNodes "resource" (xpContent xpId)

-- Sends a (synchronous) IQ set request for a (`Just') given or server-generated
-- resource and extract the JID from the non-error response.
xmppBind  :: Maybe Text -> XmppConMonad Jid
xmppBind rsrc = do
    answer <- xmppSendIQ' "bind" Nothing Set Nothing (bindBody rsrc)
    jid <- case () of () | Right IQResult{iqResultPayload = Just b} <- answer
                         , Right jid <- unpickleElem jidP b
                           -> return jid
                         | otherwise -> throw $ StreamXMLError
                                                  "Bind could'nt unpickle JID"
    modify (\s -> s{sJid = Just jid})
    return jid
  where
    -- Extracts the character data in the `jid' element.
    jidP :: PU [Node] Jid
    jidP = xpBind $ xpElemNodes "jid" (xpContent xpPrim)

-- A `bind' element pickler.
xpBind  :: PU [Node] b -> PU [Node] b
xpBind c = xpElemNodes "{urn:ietf:params:xml:ns:xmpp-bind}bind" c
