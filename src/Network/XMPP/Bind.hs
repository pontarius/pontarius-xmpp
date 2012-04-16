{-# LANGUAGE OverloadedStrings #-}

module Network.XMPP.Bind where

import Data.Text as Text

import Data.XML.Pickle
import Data.XML.Types

import Network.XMPP.Types
import Network.XMPP.Pickle
import Network.XMPP.Concurrent

bindP  :: PU [Node] b -> PU [Node] b
bindP c = xpElemNodes "{urn:ietf:params:xml:ns:xmpp-bind}bind" c

bindBody :: Maybe Text -> Element
bindBody rsrc = (pickleElem
                    (bindP . xpOption $ xpElemNodes "resource" (xpContent xpId))
                     rsrc
                 )

jidP :: PU [Node] JID
jidP = bindP $ xpElemNodes "jid" (xpContent xpPrim)

xmppThreadedBind  :: Maybe Text -> XMPPThread Text
xmppThreadedBind rsrc = do
   answer <- sendIQ' Nothing Set Nothing (bindBody rsrc)
   let (Right IQResult{iqResultPayload = Just b}) = answer -- TODO: Error handling
   let Right (JID _n _d (Just r)) = unpickleElem jidP b
   return r



