{-# LANGUAGE OverloadedStrings #-}

-- TODO: Allow the client to retry the bind with another resource

module Network.XMPP.Bind where

import Data.Text as Text

import Data.XML.Pickle
import Data.XML.Types

import Network.XMPP.Types
import Network.XMPP.Pickle
import Network.XMPP.Monad


-- A `bind' element.

bindP  :: PU [Node] b -> PU [Node] b
bindP c = xpElemNodes "{urn:ietf:params:xml:ns:xmpp-bind}bind" c


-- If the (optional resource) parameter is a `Just' value, a
-- `resource' child element will be added to the `bind' element.

bindBody :: Maybe Text -> Element
bindBody rsrc = (pickleElem
                    (bindP . xpOption $ xpElemNodes "resource" (xpContent xpId))
                     rsrc
                 )

-- Extracts the character data in the `jid' element.

jidP :: PU [Node] JID
jidP = bindP $ xpElemNodes "jid" (xpContent xpPrim)


-- Sends a (synchronous) IQ set request for a (`Just') given or
-- server-generated resource and extract the JID from the non-error
-- response.
xmppBind  :: Maybe Text -> XMPPConMonad Text
xmppBind rsrc = do
   answer <- xmppSendIQ' "bind" Nothing Set Nothing (bindBody rsrc)
   let (Right IQResult{iqResultPayload = Just b}) = answer -- TODO: Error handling
   let Right (JID _n _d (Just r)) = unpickleElem jidP b
   return r

