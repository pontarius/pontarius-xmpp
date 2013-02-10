{-# OPTIONS_HADDOCK hide #-}
{-# LANGUAGE NoMonomorphismRestriction, OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Network.Xmpp.Stream where

import           Control.Applicative ((<$>), (<*>))
import qualified Control.Exception as Ex
import           Control.Monad.Error
import           Control.Monad.Reader
import           Control.Monad.State.Strict

import qualified Data.ByteString as BS
import           Data.Conduit
import qualified Data.Conduit.Internal as DCI
import           Data.Conduit.List as CL
import           Data.Maybe (fromJust, isJust, isNothing)
import           Data.Text as Text
import           Data.Void (Void)
import           Data.XML.Pickle
import           Data.XML.Types

import           Network.Xmpp.Connection_
import           Network.Xmpp.Pickle
import           Network.Xmpp.Types
import           Network.Xmpp.Marshal

import           Text.Xml.Stream.Elements
import           Text.XML.Stream.Parse as XP

-- import Text.XML.Stream.Elements

mbl :: Maybe [a] -> [a]
mbl (Just l) = l
mbl Nothing = []

lmb :: [t] -> Maybe [t]
lmb [] = Nothing
lmb x = Just x

-- Unpickles and returns a stream element.
streamUnpickleElem :: PU [Node] a
                   -> Element
                   -> StreamSink a
streamUnpickleElem p x = do
    case unpickleElem p x of
        Left l -> throwError $ XmppOtherFailure -- TODO: Log: StreamXmlError (show l)
        Right r -> return r

-- This is the conduit sink that handles the stream XML events. We extend it
-- with ErrorT capabilities.
type StreamSink a = ErrorT XmppFailure (Pipe Event Event Void () IO) a

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
        _ -> throwError $ XmppOtherFailure

-- Sends the initial stream:stream element and pulls the server features. If the
-- server responds in a way that is invalid, an appropriate stream error will be
-- generated, the connection to the server will be closed, and a XmppFailure
-- will be produced.
startStream :: StateT Connection IO (Either XmppFailure ())
startStream = runErrorT $ do
    state <- lift $ get
    con <- liftIO $ mkConnection state
    -- Set the `from' (which is also the expected to) attribute depending on the
    -- state of the connection.
    let expectedTo = case cState state of
                 ConnectionPlain -> if cJidWhenPlain state
                                        then cJid state else Nothing
                 ConnectionSecured -> cJid state
    case cHostName state of
        Nothing -> throwError XmppOtherFailure -- TODO: When does this happen?
        Just hostname -> lift $ do
            pushXmlDecl
            pushOpenElement $
                pickleElem xpStream ( "1.0"
                                    , expectedTo
                                    , Just (Jid Nothing hostname Nothing)
                                    , Nothing
                                    , cPreferredLang state
                                    )
    response <- ErrorT $ runEventsSink $ runErrorT $ streamS expectedTo
    case response of
      Left e -> throwError e
      -- Successful unpickling of stream element.
      Right (Right (ver, from, to, id, lt, features))
        | (unpack ver) /= "1.0" ->
            closeStreamWithError con StreamUnsupportedVersion Nothing
        | lt == Nothing ->
            closeStreamWithError con StreamInvalidXml Nothing
        -- If `from' is set, we verify that it's the correct one. TODO: Should we check against the realm instead?
        | isJust from && (from /= Just (Jid Nothing (fromJust $ cHostName state) Nothing)) ->
            closeStreamWithError con StreamInvalidFrom Nothing
        | to /= expectedTo ->
            closeStreamWithError con StreamUndefinedCondition (Just $ Element "invalid-to" [] []) -- TODO: Suitable?
        | otherwise -> do
            modify (\s -> s{ cFeatures = features
                           , cStreamLang = lt
                           , cStreamId = id
                           , cFrom = from
                         } )
            return ()
      -- Unpickling failed - we investigate the element.
      Right (Left (Element name attrs children))
        | (nameLocalName name /= "stream") ->
            closeStreamWithError con StreamInvalidXml Nothing
        | (nameNamespace name /= Just "http://etherx.jabber.org/streams") ->
            closeStreamWithError con StreamInvalidNamespace Nothing
        | (isJust $ namePrefix name) && (fromJust (namePrefix name) /= "stream") ->
            closeStreamWithError con StreamBadNamespacePrefix Nothing
        | otherwise -> ErrorT $ checkchildren con (flattenAttrs attrs)
  where
    -- closeStreamWithError :: MonadIO m => TMVar Connection -> StreamErrorCondition ->
    --                         Maybe Element -> ErrorT XmppFailure m ()
    closeStreamWithError con sec el = do
        liftIO $ do
            withConnection (pushElement . pickleElem xpStreamError $
                                StreamErrorInfo sec Nothing el) con
            closeStreams con
        throwError XmppOtherFailure
    checkchildren con children =
        let to'  = lookup "to"      children
            ver' = lookup "version" children
            xl   = lookup xmlLang   children
          in case () of () | Just (Nothing :: Maybe Jid) == (safeRead <$> to') ->
                               runErrorT $ closeStreamWithError con
                                   StreamBadNamespacePrefix Nothing
                           | Nothing == ver' ->
                               runErrorT $ closeStreamWithError con
                                   StreamUnsupportedVersion Nothing
                           | Just (Nothing :: Maybe LangTag) == (safeRead <$> xl) ->
                               runErrorT $ closeStreamWithError con
                                   StreamInvalidXml Nothing
                           | otherwise ->
                               runErrorT $ closeStreamWithError con
                                   StreamBadFormat Nothing
    safeRead x = case reads $ Text.unpack x of
        [] -> Nothing
        [(y,_),_] -> Just y

flattenAttrs :: [(Name, [Content])] -> [(Name, Text.Text)]
flattenAttrs attrs = Prelude.map (\(name, content) ->
                             ( name
                             , Text.concat $ Prelude.map uncontentify content)
                             )
                         attrs
  where
    uncontentify (ContentText t) = t
    uncontentify _ = ""

-- Sets a new Event source using the raw source (of bytes)
-- and calls xmppStartStream.
restartStream :: StateT Connection IO (Either XmppFailure ())
restartStream = do
    raw <- gets (cRecv . cHandle)
    let newSource = DCI.ResumableSource (loopRead raw $= XP.parseBytes def)
                                        (return ())
    modify (\s -> s{cEventSource = newSource })
    startStream
  where
    loopRead read = do
        bs <- liftIO (read 4096)
        if BS.null bs
            then return ()
            else yield bs >> loopRead read

-- Reads the (partial) stream:stream and the server features from the stream.
-- Returns the (unvalidated) stream attributes, the unparsed element, or
-- throwError throws a `XmppOtherFailure' (if something other than an element
-- was encountered at first, or if something other than stream features was
-- encountered second).
-- TODO: from.
streamS :: Maybe Jid -> StreamSink (Either Element ( Text
                                                   , Maybe Jid
                                                   , Maybe Jid
                                                   , Maybe Text
                                                   , Maybe LangTag
                                                   , ServerFeatures ))
streamS expectedTo = do
    header <- xmppStreamHeader
    case header of
      Right (version, from, to, id, langTag) -> do
        features <- xmppStreamFeatures
        return $ Right (version, from, to, id, langTag, features)
      Left el -> return $ Left el
  where
    xmppStreamHeader :: StreamSink (Either Element (Text, Maybe Jid, Maybe Jid, Maybe Text.Text, Maybe LangTag))
    xmppStreamHeader = do
        lift throwOutJunk
        -- Get the stream:stream element (or whatever it is) from the server,
        -- and validate what we get.
        el <- openElementFromEvents -- May throw `XmppOtherFailure' if an
                                    -- element is not received
        case unpickleElem xpStream el of
            Left _ -> return $ Left el
            Right r -> return $ Right r
    xmppStreamFeatures :: StreamSink ServerFeatures
    xmppStreamFeatures = do
        e <- lift $ elements =$ CL.head
        case e of
            Nothing -> throwError XmppOtherFailure
            Just r -> streamUnpickleElem xpStreamFeatures r


xpStream :: PU [Node] (Text, Maybe Jid, Maybe Jid, Maybe Text, Maybe LangTag)
xpStream = ("xpStream","") <?+> xpElemAttrs
    (Name "stream" (Just "http://etherx.jabber.org/streams") (Just "stream"))
    (xp5Tuple
         (xpAttr "version" xpId)
         (xpAttrImplied "from" xpPrim)
         (xpAttrImplied "to" xpPrim)
         (xpAttrImplied "id" xpId)
         xpLangTag
    )

-- Pickler/Unpickler for the stream features - TLS, SASL, and the rest.
xpStreamFeatures :: PU [Node] ServerFeatures
xpStreamFeatures = ("xpStreamFeatures", "") <?+> xpWrap
    (\(tls, sasl, rest) -> SF tls (mbl sasl) rest)
    (\(SF tls sasl rest) -> (tls, lmb sasl, rest))
    (xpElemNodes
         (Name
             "features"
             (Just "http://etherx.jabber.org/streams")
             (Just "stream")
         )
         (xpTriple
              (xpOption pickleTlsFeature)
              (xpOption pickleSaslFeature)
              (xpAll xpElemVerbatim)
         )
    )
  where
    pickleTlsFeature :: PU [Node] Bool
    pickleTlsFeature = xpElemNodes "{urn:ietf:params:xml:ns:xmpp-tls}starttls"
        (xpElemExists "required")
    pickleSaslFeature :: PU [Node] [Text]
    pickleSaslFeature =  xpElemNodes
        "{urn:ietf:params:xml:ns:xmpp-sasl}mechanisms"
        (xpAll $ xpElemNodes
             "{urn:ietf:params:xml:ns:xmpp-sasl}mechanism" (xpContent xpId))
