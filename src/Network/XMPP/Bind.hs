module Network.XMPP.Bind where

import Control.Monad.Trans.State

import Data.Text as Text
import Data.XML.Types

import Network.XMPP.Monad
import Network.XMPP.Types
import Network.XMPP.Marshal

bindSt resource= SIQ $ IQ Nothing Nothing "bind" Set
           (Element "{urn:ietf:params:xml:ns:xmpp-bind}bind"
            []
            (maybe [] (return . textToNode)  resource))


xmppBind = do
  res <- gets sResource
  push $ bindSt res
  SIQ (IQ Nothing Nothing _ Result r) <- pull
  (JID n d (Just r)) <- case r of
    Element "{urn:ietf:params:xml:ns:xmpp-bind}bind" []
       [NodeElement
        jid@(Element  "{urn:ietf:params:xml:ns:xmpp-bind}jid" [] _)] ->
         return . fromText . Text.concat . elementText $  jid
    _  -> error $ "bind failed:" ++ show r
  modify (\s -> s{sResource = Just r})


