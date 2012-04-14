{-# LANGUAGE NoMonomorphismRestriction, OverloadedStrings #-}
{-# LANGUAGE TupleSections         #-}

module Network.XMPP.Stream where

import Control.Applicative((<$>))
import Control.Monad(unless)
import Control.Monad.Trans.State

import Data.Conduit
import Data.Conduit.List as CL
import Data.Text as T
import Data.XML.Pickle
import Data.XML.Types

import Network.XMPP.Monad
import Network.XMPP.Pickle
import Network.XMPP.Types

import Text.XML.Stream.Elements
import Text.XML.Stream.Parse as XP

-- import Text.XML.Stream.Elements

throwOutJunk :: Monad m => Sink Event m ()
throwOutJunk = do
  next <- CL.peek
  case next of
    Nothing -> return ()
    Just (EventBeginElement _ _) -> return ()
    _ -> CL.drop 1 >> throwOutJunk

openElementFromEvents :: Monad m => Sink Event m Element
openElementFromEvents = do
  throwOutJunk
  Just (EventBeginElement name attrs) <- CL.head
  return $ Element name attrs []


xmppStartStream :: XMPPConMonad ()
xmppStartStream = do
  hostname <- gets sHostname
  pushOpen $ pickleElem pickleStream ("1.0",Nothing, Just hostname)
  features <- pulls xmppStream
  modify (\s -> s {sFeatures = features})
  return ()

xmppRestartStream :: XMPPConMonad ()
xmppRestartStream = do
  raw <- gets sRawSrc
  let newsrc = raw $= XP.parseBytes def
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

pickleStream :: PU [Node] (Text, Maybe Text, Maybe Text)
pickleStream = xpElemAttrs (Name "stream"  (Just "http://etherx.jabber.org/streams") (Just "stream"))
      (xpTriple
       (xpAttr "version" xpId)
       (xpOption $ xpAttr "from" xpId)
       (xpOption $ xpAttr "to" xpId)
       )

pickleTLSFeature :: PU [Node] Bool
pickleTLSFeature = xpElemNodes "{urn:ietf:params:xml:ns:xmpp-tls}starttls"
                      (xpElemExists "required")

pickleSaslFeature :: PU [Node] [Text]
pickleSaslFeature =  xpElemNodes "{urn:ietf:params:xml:ns:xmpp-sasl}mechanisms"
                       (xpAll $ xpElemNodes
                        "{urn:ietf:params:xml:ns:xmpp-sasl}mechanism" (xpContent xpId) )

pickleStreamFeatures :: PU [Node] ServerFeatures
pickleStreamFeatures = xpWrap ( \(tls, sasl, rest) -> SF tls (mbl sasl) rest)
                              (\(SF tls sasl rest) -> (tls, lmb sasl, rest))
                               $
    xpElemNodes (Name "features"  (Just "http://etherx.jabber.org/streams") (Just "stream"))
      (xpTriple
        (xpOption pickleTLSFeature)
        (xpOption pickleSaslFeature)
        (xpAll xpElemVerbatim)
      )

