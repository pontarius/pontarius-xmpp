{-# LANGUAGE NoMonomorphismRestriction, OverloadedStrings #-}
{-# LANGUAGE TupleSections         #-}

module Network.XMPP.Stream where

import qualified Control.Exception as Ex
import           Control.Monad.Error
import           Control.Monad.State.Strict

import           Data.Conduit
import           Data.Conduit.BufferedSource
import           Data.Conduit.List as CL
import           Data.Text as T
import           Data.XML.Pickle
import           Data.XML.Types
import           Data.Void(Void)

import           Network.XMPP.Monad
import           Network.XMPP.Pickle
import           Network.XMPP.Types

import           Text.XML.Stream.Elements
import           Text.XML.Stream.Parse as XP

-- import Text.XML.Stream.Elements

streamUnpickleElem :: PU [Node] a
                   -> Element
                   -> ErrorT StreamError (Pipe Event Void IO) a
streamUnpickleElem p x = do
  case unpickleElem p x of
    Left l -> throwError $ StreamXMLError l
    Right r -> return r

type StreamSink a =  ErrorT StreamError (Pipe Event Void IO) a

throwOutJunk :: Monad m => Sink Event m ()
throwOutJunk = do
  next <- CL.peek
  case next of
    Nothing -> return ()
    Just (EventBeginElement _ _) -> return ()
    _ -> CL.drop 1 >> throwOutJunk

openElementFromEvents :: StreamSink Element
openElementFromEvents = do
  lift throwOutJunk
  hd <- lift CL.head
  case hd of
    Just (EventBeginElement name attrs) -> return $ Element name attrs []
    _ -> throwError $ StreamConnectionError

xmppStartStream :: XMPPConMonad (Either StreamError ())
xmppStartStream = runErrorT $ do
  hostname' <- gets sHostname
  case hostname' of
    Nothing -> throwError StreamConnectionError
    Just hostname -> lift . pushOpen $
                       pickleElem pickleStream ("1.0",Nothing, Just hostname)
  features <- ErrorT . pullSink $ runErrorT xmppStream
  modify (\s -> s {sFeatures = features})
  return ()

xmppRestartStream :: XMPPConMonad (Either StreamError ())
xmppRestartStream = do
  raw <- gets sRawSrc
  newsrc <- liftIO . bufferSource $ raw $= XP.parseBytes def
  modify (\s -> s{sConSrc = newsrc})
  xmppStartStream


xmppStream :: StreamSink ServerFeatures
xmppStream = do
  xmppStreamHeader
  xmppStreamFeatures

xmppStreamHeader :: StreamSink ()
xmppStreamHeader = do
  lift $ throwOutJunk
  (ver, _, _) <- streamUnpickleElem pickleStream =<< openElementFromEvents
  unless (ver == "1.0") . throwError $ StreamWrongVersion ver
  return()


xmppStreamFeatures :: StreamSink ServerFeatures
xmppStreamFeatures = do
    e <- lift $ elements =$ CL.head
    case e of
        Nothing -> liftIO $ Ex.throwIO XmppNoConnection
        Just r -> streamUnpickleElem pickleStreamFeatures r

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

