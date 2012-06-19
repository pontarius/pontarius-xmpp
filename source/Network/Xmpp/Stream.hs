{-# LANGUAGE NoMonomorphismRestriction, OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Network.Xmpp.Stream where

import           Control.Applicative ((<$>), (<*>))
import qualified Control.Exception as Ex
import           Control.Monad.Error
import           Control.Monad.State.Strict

import           Data.Conduit
import           Data.Conduit.BufferedSource
import           Data.Conduit.List as CL
import           Data.Maybe (fromJust, isJust, isNothing)
import           Data.Text as T
import           Data.XML.Pickle
import           Data.XML.Types
import           Data.Void(Void)

import           Network.Xmpp.Monad
import           Network.Xmpp.Pickle
import           Network.Xmpp.Types

import           Text.XML.Stream.Elements
import           Text.XML.Stream.Parse as XP

-- import Text.XML.Stream.Elements

-- Unpickles and returns a stream element. Throws a StreamXMLError on failure.
streamUnpickleElem :: PU [Node] a
                   -> Element
                   -> ErrorT StreamError (Pipe Event Void IO) a
streamUnpickleElem p x = do
    case unpickleElem p x of
        Left l -> throwError $ StreamXMLError l
        Right r -> return r

-- This is the conduit sink that handles the stream XML events. We extend it
-- with ErrorT capabilities.
type StreamSink a = ErrorT StreamError (Pipe Event Void IO) a

-- Discards all events before the first EventBeginElement.
throwOutJunk :: Monad m => Sink Event m ()
throwOutJunk = do
    next <- CL.peek
    case next of
        Nothing -> return () -- This will only happen if the stream is closed.
        Just (EventBeginElement _ _) -> return ()
        _ -> CL.drop 1 >> throwOutJunk

-- Returns an (empty) Element from a stream of XML events.
openElementFromEvents :: StreamSink Element
openElementFromEvents = do
    lift throwOutJunk
    hd <- lift CL.head
    case hd of
        Just (EventBeginElement name attrs) -> return $ Element name attrs []
        _ -> throwError $ StreamConnectionError

-- Sends the initial stream:stream element and pulls the server features.
xmppStartStream :: XmppConMonad (Either StreamError ())
xmppStartStream = runErrorT $ do
    state <- get
    -- Set the `to' attribute depending on the state of the connection.
    let to = case sConnectionState state of
                 XmppConnectionPlain -> if sJidWhenPlain state
                                        then sJid state else Nothing
                 XmppConnectionSecured -> sJid state
    case sHostname state of
        Nothing -> throwError StreamConnectionError
        Just hostname -> lift $ do
            pushXmlDecl
            pushOpenElement $
                pickleElem pickleOutStream ( to
                                           , Just hostname
                                           , sPreferredLang state
                                           )
    (lt, from, id, features) <- ErrorT . pullToSink $ runErrorT $ xmppStream to
    modify (\s -> s { sFeatures = features
                    , sStreamLang = Just lt
                    , sStreamId = id
                    , sFrom = from
                    }
           )
    return ()

-- Creates a new connection source (of Events) using the raw source (of bytes)
-- and calls xmppStartStream.
xmppRestartStream :: XmppConMonad (Either StreamError ())
xmppRestartStream = do
    raw <- gets sRawSrc
    newsrc <- liftIO . bufferSource $ raw $= XP.parseBytes def
    modify (\s -> s{sConSrc = newsrc})
    xmppStartStream

-- Reads the (partial) stream:stream and the server features from the stream.
-- Also validates the stream element's attributes and throws an error if
-- appropriate.
-- TODO: from.
xmppStream :: Maybe Jid -> StreamSink ( LangTag
                                      , Maybe Text
                                      , Maybe Text
                                      , ServerFeatures)
xmppStream expectedTo = do
    (langTag, from, id) <- xmppStreamHeader
    features <- xmppStreamFeatures
    return (langTag, from, id, features)
  where
    xmppStreamHeader :: StreamSink (LangTag, Maybe Text, Maybe Text)
    xmppStreamHeader = do
        lift throwOutJunk
        -- Get the stream:stream element (or whatever it is) from the server,
        -- and validate what we get.
        ((Name lname ns prefix), (from, id, to, ver, lang), ())
            <- streamUnpickleElem pickleInStream =<< openElementFromEvents
        unless (lname == "stream") $ throwError $ StreamNotStreamElement lname
        unless (ns == Just "http://etherx.jabber.org/streams") $ throwError $ StreamInvalidStreamNamespace ns
        unless (prefix == Just "stream") $ throwError $ StreamInvalidStreamPrefix prefix
        unless (isNothing to || (fromText $ fromJust to) == expectedTo) $ throwError $ StreamWrongTo to
        unless (ver == Just "1.0") $ throwError $ StreamWrongVersion ver
        let lang_ = maybe Nothing langTag lang
        when (isNothing lang_) $ throwError $ StreamWrongLangTag lang
        return (fromJust lang_, from, id)
    xmppStreamFeatures :: StreamSink ServerFeatures
    xmppStreamFeatures = do
        e <- lift $ elements =$ CL.head
        case e of
            Nothing -> liftIO $ Ex.throwIO StreamConnectionError
            Just r -> streamUnpickleElem xpStreamFeatures r

-- Pickler for the stream element to be sent to the server. We follow what RFC
-- 6120 calls the "prefix-free canonicalization style".)
pickleOutStream :: PU [Node] ( Maybe Jid -- from
                             , Maybe Text -- to
                             , Maybe LangTag -- xml:lang
                             )
pickleOutStream = xpWrap
    (\(from, to, _ver, lang) -> (from, to, lang))
    (\(from, to, lang) ->
         (from, to, "1.0", lang)
    )
    (xpElemAttrs
         (Name
              "stream"
              (Just "http://etherx.jabber.org/streams")
              (Just "stream")
         )
         (xp4Tuple
              (xpAttrImplied "from" xpPrim)
              (xpAttrImplied "to" xpId)
              (xpAttr "version" xpId)
              xpLangTag
         )
    )

-- Unpickler for the stream element to be received from the server. As this
-- function puts no restrictions on the element received, the data need to be
-- validated externally.
pickleInStream :: PU [Node] ( Name
                            , ( Maybe Text -- from
                              , Maybe Text -- id
                              , Maybe Text -- to
                              , Maybe Text -- version
                              , Maybe Text -- xml:lang
                              )
                            , ()
                            )
pickleInStream = xpElemWithName
    (xp5Tuple
         (xpAttrImplied "from" xpId)
         (xpAttrImplied "id" xpId)
         (xpAttrImplied "to" xpId)
         (xpAttrImplied "version" xpId)
         (xpAttrImplied (Name "lang" (Just "http://www.w3.org/XML/1998/namespace") (Just "xml")) xpId)
    )
    xpUnit
 
-- Pickler/Unpickler for the stream features - TLS, SASL, and the rest.
xpStreamFeatures :: PU [Node] ServerFeatures
xpStreamFeatures = xpWrap
    (\(tls, sasl, rest) -> SF tls (mbl sasl) rest)
    (\(SF tls sasl rest) -> (tls, lmb sasl, rest))
    (xpElemNodes
         (Name
             "features"
             (Just "http://etherx.jabber.org/streams")
             (Just "stream")
         )
         (xpTriple
              (xpOption pickleTLSFeature)
              (xpOption pickleSaslFeature)
              (xpAll xpElemVerbatim)
         )
    )
  where
    pickleTLSFeature :: PU [Node] Bool
    pickleTLSFeature = xpElemNodes "{urn:ietf:params:xml:ns:xmpp-tls}starttls"
        (xpElemExists "required")
    pickleSaslFeature :: PU [Node] [Text]
    pickleSaslFeature =  xpElemNodes
        "{urn:ietf:params:xml:ns:xmpp-sasl}mechanisms"
        (xpAll $ xpElemNodes
             "{urn:ietf:params:xml:ns:xmpp-sasl}mechanism" (xpContent xpId))