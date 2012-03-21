{-# LANGUAGE NoMonomorphismRestriction, OverloadedStrings #-}
{-# LANGUAGE TupleSections         #-}

module Network.XMPP.Stream where

import Control.Applicative((<$>))
import Control.Monad(unless)
import Control.Monad.Trans.Class
import Control.Monad.Trans.State
import Control.Monad.IO.Class

import Network.XMPP.Monad
import Network.XMPP.Pickle
import Network.XMPP.Types

import Data.Conduit
import Data.Conduit.Hexpat as HXC
import Data.Conduit.List as CL
import qualified Data.List as L
import Data.Text as T

import Text.XML.Expat.Pickle

-- import Text.XML.Stream.Elements


xmppStartStream = do
  hostname <- gets sHostname
  pushOpen $ pickleElem pickleStream ("1.0",Nothing, Just hostname)
  features <- pulls xmppStream
  modify (\s -> s {sFeatures = features})
  return ()

xmppRestartStream = do
  raw <- gets sRawSrc
  src <- gets sConSrc
  newsrc <- lift (bufferSource $ raw $= HXC.parseBS parseOpts)
  modify (\s -> s{sConSrc = newsrc})
  xmppStartStream


xmppStream :: Sink Event IO ServerFeatures
xmppStream = do
  xmppStreamHeader
  xmppStreamFeatures

xmppStreamHeader :: Sink Event IO ()
xmppStreamHeader = do
  throwOutJunk
  (ver, _, _) <- unpickleElem pickleStream <$> openElementFromEvents
  unless (ver == "1.0")  $ error  "Not XMPP version 1.0 "
  return()


xmppStreamFeatures :: Sink Event IO ServerFeatures
xmppStreamFeatures = unpickleElem pickleStreamFeatures <$> elementFromEvents


-- Pickling

pickleStream = xpWrap (snd, (((),()),)) .
  xpElemAttrs "stream:stream" $
    xpPair
      (xpPair
        (xpAttrFixed "xmlns" "jabber:client" )
        (xpAttrFixed "xmlns:stream" "http://etherx.jabber.org/streams" )
      )
      (xpTriple
       (xpAttr "version" xpText)
       (xpOption $ xpAttr "from" xpText)
       (xpOption $ xpAttr "to" xpText)
       )

pickleTLSFeature = ignoreAttrs $
  xpElem "starttls"
    (xpAttrFixed "xmlns" "urn:ietf:params:xml:ns:xmpp-tls")
    (xpElemExists "required")

pickleSaslFeature = ignoreAttrs $
  xpElem "mechanisms"
    (xpAttrFixed "xmlns" "urn:ietf:params:xml:ns:xmpp-sasl")
    (xpList0 $
     xpElemNodes "mechanism" (xpContent xpText) )

pickleStreamFeatures = xpWrap ( \(tls, sasl, rest) -> SF tls (mbl sasl) rest
                              , (\(SF tls sasl rest) -> (tls, lmb sasl, rest))
                              ) $
    xpElemNodes "stream:features"
      (xpTriple
        (xpOption pickleTLSFeature)
        (xpOption pickleSaslFeature)
        xpTrees
      )

