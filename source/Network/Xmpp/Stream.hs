{-# OPTIONS_HADDOCK hide #-}

{-# LANGUAGE NoMonomorphismRestriction, OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Network.Xmpp.Stream where

import           Control.Applicative ((<$>), (<*>))
import qualified Control.Exception as Ex
import           Control.Exception.Base
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

import           Network.Xmpp.Types
import           Network.Xmpp.Marshal

import           Text.XML.Stream.Parse as XP
import           Control.Concurrent (forkIO, threadDelay)

import Network
import Control.Concurrent.STM

import           Data.ByteString as BS
import           Data.ByteString.Base64
import           System.Log.Logger
import qualified GHC.IO.Exception as GIE
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class
import           System.IO.Error (tryIOError)
import           System.IO
import           Data.Conduit
import           Data.Conduit.Binary as CB
import           Data.Conduit.Internal as DCI
import qualified Data.Conduit.List as CL
import qualified Data.Text as T
import           Data.ByteString.Char8 as BSC8
import           Text.XML.Unresolved(InvalidEventStream(..))
import qualified Control.Exception.Lifted as ExL

import           Control.Monad.Trans.Resource as R
import           Network.Xmpp.Utilities

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
startStream :: StateT Stream IO (Either XmppFailure ())
startStream = runErrorT $ do
    state <- lift $ get
    stream <- liftIO $ mkStream state
    -- Set the `from' (which is also the expected to) attribute depending on the
    -- state of the stream.
    let expectedTo = case (streamState state, toJid $ streamConfiguration state) of
          (Plain, (Just (jid, True))) -> Just jid
          (Secured, (Just (jid, _))) -> Just jid
          (Plain, Nothing) -> Nothing
          (Secured, Nothing) -> Nothing
    case streamHostname state of
        Nothing -> throwError XmppOtherFailure -- TODO: When does this happen?
        Just hostname -> lift $ do
            pushXmlDecl
            pushOpenElement $
                pickleElem xpStream ( "1.0"
                                    , expectedTo
                                    , Just (Jid Nothing hostname Nothing)
                                    , Nothing
                                    , preferredLang $ streamConfiguration state
                                    )
    response <- ErrorT $ runEventsSink $ runErrorT $ streamS expectedTo
    case response of
      Left e -> throwError e
      -- Successful unpickling of stream element.
      Right (Right (ver, from, to, id, lt, features))
        | (T.unpack ver) /= "1.0" ->
            closeStreamWithError stream StreamUnsupportedVersion Nothing
        | lt == Nothing ->
            closeStreamWithError stream StreamInvalidXml Nothing
        -- If `from' is set, we verify that it's the correct one. TODO: Should we check against the realm instead?
        | isJust from && (from /= Just (Jid Nothing (fromJust $ streamHostname state) Nothing)) ->
            closeStreamWithError stream StreamInvalidFrom Nothing
        | to /= expectedTo ->
            closeStreamWithError stream StreamUndefinedCondition (Just $ Element "invalid-to" [] []) -- TODO: Suitable?
        | otherwise -> do
            modify (\s -> s{ streamFeatures = features
                           , streamLang = lt
                           , streamId = id
                           , streamFrom = from
                         } )
            return ()
      -- Unpickling failed - we investigate the element.
      Right (Left (Element name attrs children))
        | (nameLocalName name /= "stream") ->
            closeStreamWithError stream StreamInvalidXml Nothing
        | (nameNamespace name /= Just "http://etherx.jabber.org/streams") ->
            closeStreamWithError stream StreamInvalidNamespace Nothing
        | (isJust $ namePrefix name) && (fromJust (namePrefix name) /= "stream") ->
            closeStreamWithError stream StreamBadNamespacePrefix Nothing
        | otherwise -> ErrorT $ checkchildren stream (flattenAttrs attrs)
  where
    -- closeStreamWithError :: MonadIO m => TMVar Stream -> StreamErrorCondition ->
    --                         Maybe Element -> ErrorT XmppFailure m ()
    closeStreamWithError stream sec el = do
        liftIO $ do
            withStream (pushElement . pickleElem xpStreamError $
                                StreamErrorInfo sec Nothing el) stream
            closeStreams stream
        throwError XmppOtherFailure
    checkchildren stream children =
        let to'  = lookup "to"      children
            ver' = lookup "version" children
            xl   = lookup xmlLang   children
          in case () of () | Just (Nothing :: Maybe Jid) == (safeRead <$> to') ->
                               runErrorT $ closeStreamWithError stream
                                   StreamBadNamespacePrefix Nothing
                           | Nothing == ver' ->
                               runErrorT $ closeStreamWithError stream
                                   StreamUnsupportedVersion Nothing
                           | Just (Nothing :: Maybe LangTag) == (safeRead <$> xl) ->
                               runErrorT $ closeStreamWithError stream
                                   StreamInvalidXml Nothing
                           | otherwise ->
                               runErrorT $ closeStreamWithError stream
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
restartStream :: StateT Stream IO (Either XmppFailure ())
restartStream = do
    raw <- gets (streamReceive . streamHandle)
    let newSource = DCI.ResumableSource (loopRead raw $= XP.parseBytes def)
                                        (return ())
    modify (\s -> s{streamEventSource = newSource })
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
                                                   , StreamFeatures ))
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
    xmppStreamFeatures :: StreamSink StreamFeatures
    xmppStreamFeatures = do
        e <- lift $ elements =$ CL.head
        case e of
            Nothing -> throwError XmppOtherFailure
            Just r -> streamUnpickleElem xpStreamFeatures r

-- | Connects to the XMPP server and opens the XMPP stream against the given
-- host name, port, and realm.
openStream :: HostName -> PortID -> Text -> StreamConfiguration -> IO (Either XmppFailure (TMVar Stream))
openStream address port hostname config = do
    stream <- connectTcp address port hostname config
    case stream of
        Right stream' -> do
            result <- withStream startStream stream'
            return $ Right stream'
        Left e -> do
            return $ Left e

-- | Send "</stream:stream>" and wait for the server to finish processing and to
-- close the connection. Any remaining elements from the server are returned.
-- Surpresses StreamEndFailure exceptions, but may throw a StreamCloseError.
closeStreams :: TMVar Stream -> IO (Either XmppFailure [Element])
closeStreams = withStream $ do
    send <- gets (streamSend . streamHandle)
    cc <- gets (streamClose . streamHandle)
    liftIO $ send "</stream:stream>"
    void $ liftIO $ forkIO $ do
        threadDelay 3000000 -- TODO: Configurable value
        (Ex.try cc) :: IO (Either Ex.SomeException ())
        return ()
    collectElems []
  where
    -- Pulls elements from the stream until the stream ends, or an error is
    -- raised.
    collectElems :: [Element] -> StateT Stream IO (Either XmppFailure [Element])
    collectElems es = do
        result <- pullElement
        case result of
            Left StreamEndFailure -> return $ Right es
            Left e -> return $ Left $ StreamCloseError (es, e)
            Right e -> collectElems (e:es)

-- Enable/disable debug output
-- This will dump all incoming and outgoing network taffic to the console,
-- prefixed with "in: " and "out: " respectively
debug :: Bool
debug = False

-- TODO: Can the TLS send/recv functions throw something other than an IO error?

wrapIOException :: IO a -> StateT Stream IO (Either XmppFailure a)
wrapIOException action = do
    r <- liftIO $ tryIOError action
    case r of
        Right b -> return $ Right b
        Left e -> return $ Left $ XmppIOException e

pushElement :: Element -> StateT Stream IO (Either XmppFailure Bool)
pushElement x = do
    send <- gets (streamSend . streamHandle)
    wrapIOException $ send $ renderElement x

-- | Encode and send stanza
pushStanza :: Stanza -> TMVar Stream -> IO (Either XmppFailure Bool)
pushStanza s = withStream' . pushElement $ pickleElem xpStanza s

-- XML documents and XMPP streams SHOULD be preceeded by an XML declaration.
-- UTF-8 is the only supported XMPP encoding. The standalone document
-- declaration (matching "SDDecl" in the XML standard) MUST NOT be included in
-- XMPP streams. RFC 6120 defines XMPP only in terms of XML 1.0.
pushXmlDecl :: StateT Stream IO (Either XmppFailure Bool)
pushXmlDecl = do
    con <- gets streamHandle
    wrapIOException $ (streamSend con) "<?xml version=\"1.0\" encoding=\"UTF-8\" ?>"

pushOpenElement :: Element -> StateT Stream IO (Either XmppFailure Bool)
pushOpenElement e = do
    sink <- gets (streamSend . streamHandle)
    wrapIOException $ sink $ renderOpenElement e

-- `Connect-and-resumes' the given sink to the stream source, and pulls a
-- `b' value.
runEventsSink :: Sink Event IO b -> StateT Stream IO (Either XmppFailure b)
runEventsSink snk = do -- TODO: Wrap exceptions?
    source <- gets streamEventSource
    (src', r) <- lift $ source $$++ snk
    modify (\s -> s{streamEventSource = src'})
    return $ Right r

pullElement :: StateT Stream IO (Either XmppFailure Element)
pullElement = do
    ExL.catches (do
        e <- runEventsSink (elements =$ await)
        case e of
            Left f -> return $ Left f
            Right Nothing -> return $ Left XmppOtherFailure -- TODO
            Right (Just r) -> return $ Right r
        )
        [ ExL.Handler (\StreamEnd -> return $ Left StreamEndFailure)
        , ExL.Handler (\(InvalidXmppXml s) -- Invalid XML `Event' encountered, or missing element close tag
                     -> return $ Left XmppOtherFailure) -- TODO: Log: s
        , ExL.Handler $ \(e :: InvalidEventStream) -- xml-conduit exception
                     -> return $ Left XmppOtherFailure -- TODO: Log: (show e)
        ]

-- Pulls an element and unpickles it.
pullUnpickle :: PU [Node] a -> StateT Stream IO (Either XmppFailure a)
pullUnpickle p = do
    elem <- pullElement
    case elem of
        Left e -> return $ Left e
        Right elem' -> do
            let res = unpickleElem p elem'
            case res of
                Left e -> return $ Left XmppOtherFailure -- TODO: Log
                Right r -> return $ Right r

-- | Pulls a stanza (or stream error) from the stream.
pullStanza :: TMVar Stream -> IO (Either XmppFailure Stanza)
pullStanza = withStream' $ do
    res <- pullUnpickle xpStreamStanza
    case res of
        Left e -> return $ Left e
        Right (Left e) -> return $ Left $ StreamErrorFailure e
        Right (Right r) -> return $ Right r

-- Performs the given IO operation, catches any errors and re-throws everything
-- except 'ResourceVanished' and IllegalOperation, in which case it will return False instead
catchPush :: IO () -> IO Bool
catchPush p = ExL.catch
    (p >> return True)
    (\e -> case GIE.ioe_type e of
         GIE.ResourceVanished -> return False
         GIE.IllegalOperation -> return False
         _ -> ExL.throwIO e
    )

-- Stream state used when there is no connection.
xmppNoStream :: Stream
xmppNoStream = Stream {
      streamState = Closed
    , streamHandle = StreamHandle { streamSend = \_ -> return False
                                  , streamReceive = \_ -> ExL.throwIO
                                                          XmppOtherFailure
                                  , streamFlush = return ()
                                  , streamClose = return ()
                                  }
    , streamEventSource = DCI.ResumableSource zeroSource (return ())
    , streamFeatures = StreamFeatures Nothing [] []
    , streamHostname = Nothing
    , streamFrom = Nothing
    , streamId = Nothing
    , streamLang = Nothing
    , streamJid = Nothing
    , streamConfiguration = StreamConfiguration Nothing Nothing
    }
  where
    zeroSource :: Source IO output
    zeroSource = liftIO . ExL.throwIO $ XmppOtherFailure

connectTcp :: HostName -> PortID -> Text -> StreamConfiguration -> IO (Either XmppFailure (TMVar Stream))
connectTcp host port hostname config = do
    let PortNumber portNumber = port
    debugM "Pontarius.Xmpp" $ "Connecting to " ++ host ++ " on port " ++
        (show portNumber) ++ " through the realm " ++ (T.unpack hostname) ++ "."
    h <- connectTo host port
    debugM "Pontarius.Xmpp" "Setting NoBuffering mode on handle."
    hSetBuffering h NoBuffering
    let eSource = DCI.ResumableSource
                  ((sourceHandle h $= logConduit) $= XP.parseBytes def)
                  (return ())
    let hand = StreamHandle { streamSend = \d -> do
                                     let d64 = encode d
                                     debugM "Pontarius.Xmpp" $
                                       "Sending TCP data: " ++ (BSC8.unpack d64)
                                       ++ "."
                                     catchPush $ BS.hPut h d
                                , streamReceive = \n -> do
                                     d <- BS.hGetSome h n
                                     let d64 = encode d
                                     debugM "Pontarius.Xmpp" $
                                       "Received TCP data: " ++
                                       (BSC8.unpack d64) ++ "."
                                     return d
                                , streamFlush = hFlush h
                                , streamClose = hClose h
                                }
    let stream = Stream
            { streamState = Plain
            , streamHandle = hand
            , streamEventSource = eSource
            , streamFeatures = StreamFeatures Nothing [] []
            , streamHostname = (Just hostname)
            , streamFrom = Nothing
            , streamId = Nothing
            , streamLang = Nothing
            , streamJid = Nothing
            , streamConfiguration = config
            }
    stream' <- mkStream stream
    return $ Right stream'
  where
    logConduit :: Conduit ByteString IO ByteString
    logConduit = CL.mapM $ \d -> do
        let d64 = encode d
        debugM "Pontarius.Xmpp" $ "Received TCP data: " ++ (BSC8.unpack d64) ++
            "."
        return d


-- Closes the connection and updates the XmppConMonad Stream state.
-- killStream :: TMVar Stream -> IO (Either ExL.SomeException ())
killStream :: TMVar Stream -> IO (Either XmppFailure ())
killStream = withStream $ do
    cc <- gets (streamClose . streamHandle)
    err <- wrapIOException cc
    -- (ExL.try cc :: IO (Either ExL.SomeException ()))
    put xmppNoStream
    return err

-- Sends an IQ request and waits for the response. If the response ID does not
-- match the outgoing ID, an error is thrown.
pushIQ :: StanzaID
       -> Maybe Jid
       -> IQRequestType
       -> Maybe LangTag
       -> Element
       -> TMVar Stream
       -> IO (Either XmppFailure (Either IQError IQResult))
pushIQ iqID to tp lang body stream = do
    pushStanza (IQRequestS $ IQRequest iqID Nothing to lang tp body) stream
    res <- pullStanza stream
    case res of
        Left e -> return $ Left e
        Right (IQErrorS e) -> return $ Right $ Left e
        Right (IQResultS r) -> do
            unless
                (iqID == iqResultID r) . liftIO . ExL.throwIO $
                    XmppOtherFailure
                -- TODO: Log: ("In sendIQ' IDs don't match: " ++ show iqID ++
                -- " /= " ++ show (iqResultID r) ++ " .")
            return $ Right $ Right r
        _ -> return $ Left XmppOtherFailure
             -- TODO: Log: "sendIQ': unexpected stanza type "

debugConduit :: Pipe l ByteString ByteString u IO b
debugConduit = forever $ do
    s' <- await
    case s' of
        Just s ->  do
            liftIO $ BS.putStrLn (BS.append "in: " s)
            yield s
        Nothing -> return ()

elements :: R.MonadThrow m => Conduit Event m Element
elements = do
        x <- await
        case x of
            Just (EventBeginElement n as) -> do
                                                 goE n as >>= yield
                                                 elements
            Just (EventEndElement streamName) -> lift $ R.monadThrow StreamEnd
            Nothing -> return ()
            _ -> lift $ R.monadThrow $ InvalidXmppXml $ "not an element: " ++ show x
  where
    many' f =
        go id
      where
        go front = do
            x <- f
            case x of
                Left x -> return $ (x, front [])
                Right y -> go (front . (:) y)
    goE n as = do
        (y, ns) <- many' goN
        if y == Just (EventEndElement n)
            then return $ Element n as $ compressNodes ns
            else lift $ R.monadThrow $ InvalidXmppXml $
                                         "Missing close tag: " ++ show n
    goN = do
        x <- await
        case x of
            Just (EventBeginElement n as) -> (Right . NodeElement) <$> goE n as
            Just (EventInstruction i) -> return $ Right $ NodeInstruction i
            Just (EventContent c) -> return $ Right $ NodeContent c
            Just (EventComment t) -> return $ Right $ NodeComment t
            Just (EventCDATA t) -> return $ Right $ NodeContent $ ContentText t
            _ -> return $ Left x

    compressNodes :: [Node] -> [Node]
    compressNodes [] = []
    compressNodes [x] = [x]
    compressNodes (NodeContent (ContentText x) : NodeContent (ContentText y) : z) =
        compressNodes $ NodeContent (ContentText $ x `Text.append` y) : z
    compressNodes (x:xs) = x : compressNodes xs

    streamName :: Name
    streamName = (Name "stream" (Just "http://etherx.jabber.org/streams") (Just "stream"))

withStream :: StateT Stream IO (Either XmppFailure c) -> TMVar Stream -> IO (Either XmppFailure c)
withStream action stream = bracketOnError
                                         (atomically $ takeTMVar stream)
                                         (atomically . putTMVar stream)
                                         (\s -> do
                                               (r, s') <- runStateT action s
                                               atomically $ putTMVar stream s'
                                               return r
                                         )

-- nonblocking version. Changes to the connection are ignored!
withStream' :: StateT Stream IO (Either XmppFailure b) -> TMVar Stream -> IO (Either XmppFailure b)
withStream' action stream = do
    stream_ <- atomically $ readTMVar stream
    (r, _) <- runStateT action stream_
    return r


mkStream :: Stream -> IO (TMVar Stream)
mkStream con = {- Stream `fmap` -} (atomically $ newTMVar con)
