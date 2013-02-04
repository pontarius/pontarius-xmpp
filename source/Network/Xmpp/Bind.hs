{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_HADDOCK hide #-}

module Network.Xmpp.Bind where

import Control.Exception

import Data.Text as Text
import Data.XML.Pickle
import Data.XML.Types

import Network.Xmpp.Connection_
import Network.Xmpp.Pickle
import Network.Xmpp.Types

import Control.Monad.State(modify)

import Control.Concurrent.STM.TMVar

import Control.Monad.Error

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
xmppBind  :: Maybe Text -> TMVar Connection -> IO (Either XmppFailure Jid)
xmppBind rsrc c = runErrorT $ do
    answer <- ErrorT $ pushIQ' "bind" Nothing Set Nothing (bindBody rsrc) c
    case answer of
        Right IQResult{iqResultPayload = Just b} -> do
            let jid = unpickleElem xpJid b
            case jid of
                Right jid' -> do
                    ErrorT $ withConnection (do
                                      modify $ \s -> s{cJid = Just jid'}
                                      return $ Right jid') c -- not pretty
                    return jid'
                otherwise -> throwError XmppOtherFailure
                -- TODO: Log: ("Bind couldn't unpickle JID from " ++ show answer)
        otherwise -> throwError XmppOtherFailure
  where
    -- Extracts the character data in the `jid' element.
    xpJid :: PU [Node] Jid
    xpJid = xpBind $ xpElemNodes jidName (xpContent xpPrim)
    jidName = "{urn:ietf:params:xml:ns:xmpp-bind}jid"

-- A `bind' element pickler.
xpBind  :: PU [Node] b -> PU [Node] b
xpBind c = xpElemNodes "{urn:ietf:params:xml:ns:xmpp-bind}bind" c
