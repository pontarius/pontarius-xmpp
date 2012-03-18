{-# LANGUAGE NoMonomorphismRestriction, OverloadedStrings #-}

module Network.XMPP.Stream where

import Control.Monad(unless)
import Control.Monad.Trans.State

import Network.XMPP.Monad

import Data.Conduit
import Data.Conduit.List as CL
import qualified Data.List as L
import Data.Text as T
import Data.XML.Types

import Text.XML.Stream.Elements

xmppStartStream = do
  hostname <- gets sHostname
  pushOpen $ streamE hostname
  features <- pulls xmppStream
  modify (\s -> s {sFeatures = features})
  return ()


xmppStream :: ResourceThrow m => Sink Event m ServerFeatures
xmppStream = do
  xmppStreamHeader
  xmppStreamFeatures


xmppStreamHeader :: Resource m => Sink Event m ()
xmppStreamHeader = do
  Just EventBeginDocument <- CL.head
  Just (EventBeginElement "{http://etherx.jabber.org/streams}stream" streamAttrs) <- CL.head
  unless (checkVersion streamAttrs)  $ error  "Not XMPP version 1.0 "
  return ()
  where
    checkVersion = L.any (\x -> (fst x == "version") && (snd x == [ContentText "1.0"]))


xmppStreamFeatures
  :: ResourceThrow m => Sink Event m ServerFeatures
xmppStreamFeatures = do
  Element "{http://etherx.jabber.org/streams}features" [] features' <- elementFromEvents
  let features = do
       f <- features'
       case f of
         NodeElement e -> [e]
         _ -> []
  let starttls = features >>= isNamed "{urn:ietf:params:xml:ns:xmpp-tls}starttls"
  let starttlsRequired = starttls
        >>= elementChildren
        >>= isNamed "{urn:ietf:params:xml:ns:xmpp-tls}required"
  let mechanisms = features
                 >>= isNamed "{urn:ietf:params:xml:ns:xmpp-sasl}mechanisms"
                 >>= elementChildren
                 >>= isNamed "{urn:ietf:params:xml:ns:xmpp-sasl}mechanism"
                 >>= elementText
  return SF { stls = not $ L.null starttls
            , stlsRequired = not $ L.null starttlsRequired
            , saslMechanisms = mechanisms
            , other = features
            }

streamE :: T.Text -> Element
streamE hostname =
  Element (Name "stream" (Just "http://etherx.jabber.org/streams") (Just "stream"))
     [
       ("xml:language" , [ContentText "en"])
     , ("version", [ContentText "1.0"])
     , ("to", [ContentText hostname])
     ]
     []


