{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_HADDOCK hide #-}

module Network.XMPP.Bind where

import Data.Text as Text

import Data.XML.Pickle
import Data.XML.Types

import Network.XMPP.Types
import Network.XMPP.Pickle
import Network.XMPP.Monad

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
xmppBind  :: Maybe Text -> XMPPConMonad Text
xmppBind rsrc = do
    answer <- xmppSendIQ' "bind" Nothing Set Nothing (bindBody rsrc)
    let Right IQResult{iqResultPayload = Just b} = answer -- TODO: Error handling
    let Right (JID _n _d (Just r)) = unpickleElem jidP b
    return r
  where
    -- Extracts the character data in the `jid' element.
    jidP :: PU [Node] JID
    jidP = xpBind $ xpElemNodes "jid" (xpContent xpPrim)

-- A `bind' element pickler.
xpBind  :: PU [Node] b -> PU [Node] b
xpBind c = xpElemNodes "{urn:ietf:params:xml:ns:xmpp-bind}bind" c