{-# LANGUAGE NoMonomorphismRestriction, OverloadedStrings #-}
{-# LANGUAGE TupleSections         #-}

module Network.XMPP.Stream where

import Control.Applicative((<$>))
import Control.Monad(unless, forever)
import Control.Monad.Trans.Class
import Control.Monad.Trans.State
import Control.Monad.IO.Class

import Network.XMPP.Monad
import Network.XMPP.Pickle
import Network.XMPP.Types

import Data.Conduit
import Data.Conduit.List as CL
import Data.Default(def)
import qualified Data.List as L
import Data.Conduit.Text as CT
import Data.Text as T
import Data.XML.Types
import Data.XML.Pickle

import qualified Text.XML.Stream.Parse as XP
import Text.XML.Stream.Elements


-- import Text.XML.Stream.Elements

throwOutJunk = do
  liftIO $ putStrLn "peeking..."
  next <- CL.peek
  liftIO $ putStrLn "peeked."
  liftIO $ do
    putStrLn "peek:"
    print next
    putStrLn "=========="
  case next of
    Nothing -> return ()
    Just (EventBeginElement _ _) -> return ()
    _ -> CL.drop 1 >> throwOutJunk

openElementFromEvents = do
  throwOutJunk
  liftIO $ putStrLn "starting ------"
  Just (EventBeginElement name attrs) <- CL.head
  return $ Element name attrs []


xmppStartStream :: XMPPMonad ()
xmppStartStream = do
  hostname <- gets sHostname
  pushOpen $ pickleElem pickleStream ("1.0",Nothing, Just hostname)
  features <- pulls xmppStream
  modify (\s -> s {sFeatures = features})
  return ()

xmppRestartStream :: XMPPMonad ()
xmppRestartStream = do
  raw <- gets sRawSrc
  src <- gets sConSrc

  newsrc <- lift (bufferSource $ raw $= XP.parseBytes def)
  modify (\s -> s{sConSrc = newsrc})
  xmppStartStream


xmppStream :: Sink Event (ResourceT IO) ServerFeatures
xmppStream = do
  xmppStreamHeader
  xmppStreamFeatures

xmppStreamHeader :: Sink Event (ResourceT IO) ()
xmppStreamHeader = do
  liftIO $ putStrLn "throwing junk!"
--  throwOutJunk
  liftIO $ putStrLn "junk thrown"
  (ver, _, _) <- unpickleElem pickleStream <$> openElementFromEvents
  unless (ver == "1.0")  $ error  "Not XMPP version 1.0 "
  return()


xmppStreamFeatures :: Sink Event (ResourceT IO) ServerFeatures
xmppStreamFeatures = unpickleElem pickleStreamFeatures <$> elementFromEvents


-- Pickling

pickleStream :: PU [Node] (Text, Maybe Text, Maybe Text)
pickleStream = xpWrap snd (((),()),) .
  xpElemAttrs "stream:stream" $
    xpPair
      (xpPair
        (xpAttrFixed "xmlns" "jabber:client" )
        (xpAttrFixed "xmlns:stream" "http://etherx.jabber.org/streams" )
      )
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
                       (xpAll $ xpElemNodes "mechanism" (xpContent xpId) )

pickleStreamFeatures :: PU [Node] ServerFeatures
pickleStreamFeatures = xpWrap ( \(tls, sasl, rest) -> SF tls (mbl sasl) rest)
                              (\(SF tls sasl rest) -> (tls, lmb sasl, rest))
                               $
    xpElemNodes "stream:features"
      (xpTriple
        (xpOption pickleTLSFeature)
        (xpOption pickleSaslFeature)
        (xpAll xpElemVerbatim)
      )

