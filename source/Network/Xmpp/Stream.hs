{-# OPTIONS_HADDOCK hide #-}

{-# LANGUAGE CPP #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Network.Xmpp.Stream where

import           Control.Applicative ((<$>))
import           Control.Concurrent (forkIO, threadDelay)
import           Control.Concurrent.STM
import qualified Control.Exception as Ex
import qualified Control.Exception.Lifted as ExL
import           Control.Monad
import           Control.Monad.Error
import           Control.Monad.State.Strict
import           Control.Monad.Trans.Resource as R
import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC8
import           Data.Char (isSpace)
import           Data.Conduit
import qualified Data.Conduit.Internal as DCI
import qualified Data.Conduit.List as CL
import           Data.IP
import           Data.List
import           Data.Maybe
import           Data.Ord
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import           Data.Void (Void)
import           Data.XML.Pickle
import           Data.XML.Types
import qualified GHC.IO.Exception as GIE
import           Network
import           Network.DNS hiding (encode, lookup)
import           Network.Xmpp.Marshal
import           Network.Xmpp.Types
import           System.IO
-- import           System.IO.Error (tryIOError) <- Only available in base >=4.4
import           System.Log.Logger
import           System.Random (randomRIO)
import           Text.XML.Stream.Parse as XP
import           Text.XML.Unresolved(InvalidEventStream(..))

import           Network.Xmpp.Utilities

-- "readMaybe" definition, as readMaybe is not introduced in the `base' package
-- until version 4.6.
readMaybe_ :: (Read a) => String -> Maybe a
readMaybe_ string = case reads string of
                        [(a, "")] -> Just a
                        _ -> Nothing

-- import Text.XML.Stream.Elements

mbl :: Maybe [a] -> [a]
mbl (Just l) = l
mbl Nothing = []

lmb :: [t] -> Maybe [t]
lmb [] = Nothing
lmb x = Just x

pushing  :: MonadIO m =>
               m (Either XmppFailure Bool)
            -> ErrorT XmppFailure m ()
pushing m = do
    res <- ErrorT m
    case res of
        True -> return ()
        False -> do
            liftIO $ debugM "Pontarius.Xmpp" "Failed to send data."
            throwError XmppOtherFailure

-- Unpickles and returns a stream element.
streamUnpickleElem :: PU [Node] a
                   -> Element
                   -> StreamSink a
streamUnpickleElem p x = do
    case unpickleElem p x of
        Left l -> do
            liftIO $ warningM "Pontarius.Xmpp" $ "streamUnpickleElem: Unpickle error: " ++ ppUnpickleError l
            throwError $ XmppOtherFailure
        Right r -> return r

-- This is the conduit sink that handles the stream XML events. We extend it
-- with ErrorT capabilities.
type StreamSink a = ErrorT XmppFailure (ConduitM Event Void IO) a

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
        _ -> do
            liftIO $ warningM "Pontarius.Xmpp" "openElementFromEvents: Stream ended."
            throwError XmppOtherFailure

-- Sends the initial stream:stream element and pulls the server features. If the
-- server responds in a way that is invalid, an appropriate stream error will be
-- generated, the connection to the server will be closed, and a XmppFailure
-- will be produced.
startStream :: StateT StreamState IO (Either XmppFailure ())
startStream = runErrorT $ do
    lift $ lift $ debugM "Pontarius.Xmpp" "Starting stream..."
    st <- lift $ get
    -- Set the `from' (which is also the expected to) attribute depending on the
    -- state of the stream.
    let expectedTo = case ( streamConnectionState st
                          , toJid $ streamConfiguration st) of
          (Plain    , (Just (jid, True)))  -> Just jid
          (Plain    , _                 )  -> Nothing
          (Secured  , (Just (jid, _   )))  -> Just jid
          (Secured  , Nothing           )  -> Nothing
          (Closed   , _                 )  -> Nothing
          (Finished , _                 )  -> Nothing
    case streamAddress st of
        Nothing -> do
            lift $ lift $ errorM "Pontarius.Xmpp" "Server sent no hostname."
            throwError XmppOtherFailure
        Just address -> do
            pushing pushXmlDecl
            pushing . pushOpenElement . streamNSHack $
                pickleElem xpStream ( "1.0"
                                    , expectedTo
                                    , Just (Jid Nothing address Nothing)
                                    , Nothing
                                    , preferredLang $ streamConfiguration st
                                    )
    response <- ErrorT $ runEventsSink $ runErrorT $ streamS expectedTo
    case response of
      Right (ver, from, to, sid, lt, features)
        | (Text.unpack ver) /= "1.0" ->
            closeStreamWithError StreamUnsupportedVersion Nothing
                "Unknown version"

    -- HACK: We ignore MUST-strength requirement (section 4.7.4. of RFC
    -- 6120) for the sake of compatibility with jabber.org
        --  | lt == Nothing ->
        --     closeStreamWithError StreamInvalidXml Nothing
        --         "Stream has no language tag"

    -- If `from' is set, we verify that it's the correct one. TODO: Should we
    -- check against the realm instead?
        | isJust from && (from /= Just (Jid Nothing (fromJust $ streamAddress st) Nothing)) ->
            closeStreamWithError StreamInvalidFrom Nothing
                "Stream from is invalid"
        | to /= expectedTo ->
            closeStreamWithError StreamUndefinedCondition (Just $ Element "invalid-to" [] [])
                "Stream to invalid"-- TODO: Suitable?
        | otherwise -> do
            -- HACK: (ignore section 4.7.4. of RFC 6120), see above
            unless (isJust lt) $ liftIO $ warningM "Pontariusm.Xmpp"
                "Stream has no language tag"
            modify (\s -> s{ streamFeatures = features
                           , streamLang = lt
                           , streamId = sid
                           , streamFrom = from
                         } )
            return ()
      -- Unpickling failed - we investigate the element.
      Left (Element name attrs _children)
        | (nameLocalName name /= "stream") ->
            closeStreamWithError StreamInvalidXml Nothing
               "Root element is not stream"
        | (nameNamespace name /= Just "http://etherx.jabber.org/streams") ->
            closeStreamWithError StreamInvalidNamespace Nothing
               "Wrong root element name space"
        | (isJust $ namePrefix name) && (fromJust (namePrefix name) /= "stream") ->
            closeStreamWithError StreamBadNamespacePrefix Nothing
                "Root name prefix set and not stream"
        | otherwise -> ErrorT $ checkchildren (flattenAttrs attrs)
  where
    -- HACK: We include the default namespace to make isode's M-LINK server happy.
    streamNSHack e = e{elementAttributes = elementAttributes e
                                           ++ [( "xmlns"
                                               , [ContentText "jabber:client"])]}
    closeStreamWithError  :: StreamErrorCondition -> Maybe Element -> String
                          -> ErrorT XmppFailure (StateT StreamState IO) ()
    closeStreamWithError sec el msg = do
        void . lift . pushElement . pickleElem xpStreamError
            $ StreamErrorInfo sec Nothing el
        void . lift $ closeStreams'
        liftIO $ errorM "Pontarius.Xmpp" $ "closeStreamWithError: " ++ msg
        throwError XmppOtherFailure
    checkchildren children =
        let to'  = lookup "to"      children
            ver' = lookup "version" children
            xl   = lookup xmlLang   children
          in case () of () | Just Nothing == fmap jidFromText to' ->
                               runErrorT $ closeStreamWithError
                                   StreamBadNamespacePrefix Nothing
                                   "stream to not a valid JID"
                           | Nothing == ver' ->
                               runErrorT $ closeStreamWithError
                                   StreamUnsupportedVersion Nothing
                                   "stream no version"
                           | Just (Nothing :: Maybe LangTag) == (safeRead <$> xl) ->
                               runErrorT $ closeStreamWithError
                                   StreamInvalidXml Nothing
                                   "stream no language tag"
                           | otherwise ->
                               runErrorT $ closeStreamWithError
                                   StreamBadFormat Nothing
                                   ""
    safeRead x = case reads $ Text.unpack x of
        [] -> Nothing
        ((y,_):_) -> Just y

flattenAttrs :: [(Name, [Content])] -> [(Name, Text.Text)]
flattenAttrs attrs = Prelude.map (\(name, cont) ->
                             ( name
                             , Text.concat $ Prelude.map uncontentify cont)
                             )
                         attrs
  where
    uncontentify (ContentText t) = t
    uncontentify _ = ""

-- Sets a new Event source using the raw source (of bytes)
-- and calls xmppStartStream.
restartStream :: StateT StreamState IO (Either XmppFailure ())
restartStream = do
    liftIO $ debugM "Pontarius.Xmpp" "Restarting stream..."
    raw <- gets streamHandle
    let newSource = sourceStreamHandle raw $= XP.parseBytes def
    buffered <- liftIO . bufferSrc $ newSource
    modify (\s -> s{streamEventSource = buffered })
    startStream


sourceStreamHandle :: MonadIO m => StreamHandle -> ConduitM i ByteString m ()
sourceStreamHandle s = loopRead $ streamReceive s
  where
    loopRead rd = do
        bs <- liftIO (rd 4096)
        if BS.null bs
            then return ()
            else do
               liftIO $ debugM "Pontarius.Xmpp" $ "in: " ++
                              (Text.unpack . Text.decodeUtf8 $ bs)
               yield bs
               loopRead rd

-- We buffer sources because we don't want to lose data when multiple
-- xml-entities are sent with the same packet and we don't want to eternally
-- block the StreamState while waiting for data to arrive
bufferSrc :: MonadIO m => Source IO o -> IO (ConduitM i o m ())
bufferSrc src = do
    ref <- newTMVarIO $ DCI.ResumableSource src (return ())
    let go = do
            dt <- liftIO $ Ex.bracketOnError (atomically $ takeTMVar ref)
                                          (\_ -> atomically . putTMVar ref $
                                                 DCI.ResumableSource zeroSource
                                                                     (return ())
                                          )
                                          (\s -> do
                                                (s', dt) <- s $$++ CL.head
                                                atomically $ putTMVar ref s'
                                                return dt
                                          )
            case dt of
                Nothing -> return ()
                Just d -> yield d >> go
    return go


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
streamS _expectedTo = do -- TODO: check expectedTo
    streamHeader <- xmppStreamHeader
    case streamHeader of
      Right (version, from, to, sid, lTag) -> do
        features <- xmppStreamFeatures
        return $ Right (version, from, to, sid, lTag, features)
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
            Nothing -> do
                lift $ lift $ errorM "Pontarius.Xmpp" "streamS: Stream ended."
                throwError XmppOtherFailure
            Just r -> streamUnpickleElem xpStreamFeatures r

-- | Connects to the XMPP server and opens the XMPP stream against the given
-- realm.
openStream :: HostName -> StreamConfiguration -> IO (Either XmppFailure (Stream))
openStream realm config = runErrorT $ do
    lift $ debugM "Pontarius.Xmpp" "Opening stream..."
    stream' <- createStream realm config
    ErrorT . liftIO $ withStream startStream stream'
    return stream'

-- | Send \"</stream:stream>\" and wait for the server to finish processing and
-- to close the connection. Any remaining elements from the server are returned.
-- Surpresses 'StreamEndFailure' exceptions, but may throw a 'StreamCloseError'.
closeStreams :: Stream -> IO ()
closeStreams = withStream closeStreams'

closeStreams' :: StateT StreamState IO ()
closeStreams' = do
    lift $ debugM "Pontarius.Xmpp" "Closing stream"
    send <- gets (streamSend . streamHandle)
    cc <- gets (streamClose . streamHandle)
    lift $ debugM "Pontarius.Xmpp" "Sending closing tag"
    void . liftIO $ send "</stream:stream>"
    lift $ debugM "Pontarius.Xmpp" "Waiting for stream to close"
    void $ liftIO $ forkIO $ do
        threadDelay 3000000 -- TODO: Configurable value
        void ((Ex.try cc) :: IO (Either Ex.SomeException ()))
        return ()
    put xmppNoStream{ streamConnectionState = Finished }
--     lift $ debugM "Pontarius.Xmpp" "Collecting remaining elements"
--     es <- collectElems []
    -- lift $ debugM "Pontarius.Xmpp" "Stream sucessfully closed"
    -- return es
  -- where
  --   -- Pulls elements from the stream until the stream ends, or an error is
  --   -- raised.
  --   collectElems :: [Element] -> StateT StreamState IO (Either XmppFailure [Element])
  --   collectElems es = do
  --       result <- pullElement
  --       case result of
  --           Left StreamEndFailure -> return $ Right es
  --           Left e -> return $ Left $ StreamCloseError (es, e)
  --           Right e -> collectElems (e:es)

-- TODO: Can the TLS send/recv functions throw something other than an IO error?
debugOut :: MonadIO m => ByteString -> m ()
debugOut outData = liftIO $ debugM "Pontarius.Xmpp"
             ("Out: " ++ (Text.unpack . Text.decodeUtf8 $ outData))

wrapIOException :: IO a -> StateT StreamState IO (Either XmppFailure a)
wrapIOException action = do
    r <- liftIO $ tryIOError action
    case r of
        Right b -> return $ Right b
        Left e -> do
            lift $ warningM "Pontarius.Xmpp" $ "wrapIOException: Exception wrapped: " ++ (show e)
            return $ Left $ XmppIOException e

pushElement :: Element -> StateT StreamState IO (Either XmppFailure Bool)
pushElement x = do
    send <- gets (streamSend . streamHandle)
    let outData = renderElement $ nsHack x
    debugOut outData
    wrapIOException $ send  outData
  where
    -- HACK: We remove the "jabber:client" namespace because it is set as
    -- default in the stream. This is to make isode's M-LINK server happy and
    -- should be removed once jabber.org accepts prefix-free canonicalization

nsHack :: Element -> Element
nsHack e@(Element{elementName = n})
    | nameNamespace n == Just "jabber:client" =
        e{ elementName = Name (nameLocalName n) Nothing Nothing
         , elementNodes = map mapNSHack $ elementNodes e
         }
    | otherwise = e
  where
    mapNSHack :: Node -> Node
    mapNSHack (NodeElement el) = NodeElement $ nsHack el
    mapNSHack nd = nd

-- | Encode and send stanza
pushStanza :: Stanza -> Stream -> IO (Either XmppFailure Bool)
pushStanza s = withStream' . pushElement $ pickleElem xpStanza s

-- XML documents and XMPP streams SHOULD be preceeded by an XML declaration.
-- UTF-8 is the only supported XMPP encoding. The standalone document
-- declaration (matching "SDDecl" in the XML standard) MUST NOT be included in
-- XMPP streams. RFC 6120 defines XMPP only in terms of XML 1.0.
pushXmlDecl :: StateT StreamState IO (Either XmppFailure Bool)
pushXmlDecl = do
    con <- gets streamHandle
    wrapIOException $ (streamSend con) "<?xml version=\"1.0\" encoding=\"UTF-8\" ?>"

pushOpenElement :: Element -> StateT StreamState IO (Either XmppFailure Bool)
pushOpenElement e = do
    send <- gets (streamSend . streamHandle)
    let outData = renderOpenElement e
    debugOut outData
    wrapIOException $ send outData

-- `Connect-and-resumes' the given sink to the stream source, and pulls a
-- `b' value.
runEventsSink :: Sink Event IO b -> StateT StreamState IO b
runEventsSink snk = do -- TODO: Wrap exceptions?
    src <- gets streamEventSource
    r <- liftIO $ src $$ snk
    return  r

pullElement :: StateT StreamState IO (Either XmppFailure Element)
pullElement = do
    ExL.catches (do
        e <- runEventsSink (elements =$ await)
        case e of
            Nothing -> do
                lift $ errorM "Pontarius.Xmpp" "pullElement: Stream ended."
                return . Left $ XmppOtherFailure
            Just r -> return $ Right r
        )
        [ ExL.Handler (\StreamEnd -> return $ Left StreamEndFailure)
        , ExL.Handler (\(InvalidXmppXml s) -- Invalid XML `Event' encountered, or missing element close tag
                     -> do
                            lift $ errorM "Pontarius.Xmpp" $ "pullElement: Invalid XML: " ++ (show s)
                            return . Left $ XmppOtherFailure)
        , ExL.Handler $ \(e :: InvalidEventStream)
                     -> do
                            lift $ errorM "Pontarius.Xmpp" $ "pullElement: Invalid event stream: " ++ (show e)
                            return . Left $ XmppOtherFailure
        ]

-- Pulls an element and unpickles it.
pullUnpickle :: PU [Node] a -> StateT StreamState IO (Either XmppFailure a)
pullUnpickle p = do
    el <- pullElement
    case el of
        Left e -> return $ Left e
        Right elem' -> do
            let res = unpickleElem p elem'
            case res of
                Left e -> do
                    lift $ errorM "Pontarius.Xmpp" $ "pullUnpickle: Unpickle failed: " ++ (ppUnpickleError e)
                    return . Left $ XmppOtherFailure
                Right r -> return $ Right r

-- | Pulls a stanza (or stream error) from the stream.
pullStanza :: Stream -> IO (Either XmppFailure Stanza)
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

zeroHandle :: StreamHandle
zeroHandle = StreamHandle { streamSend = \_ -> return False
                          , streamReceive = \_ -> do
                                 errorM "Pontarius.Xmpp"
                                        "xmppNoStream: Stream is closed."
                                 ExL.throwIO XmppOtherFailure
                          , streamFlush = return ()
                          , streamClose = return ()
                          }

-- Stream state used when there is no connection.
xmppNoStream :: StreamState
xmppNoStream = StreamState {
      streamConnectionState = Closed
    , streamHandle = zeroHandle
    , streamEventSource = zeroSource
    , streamFeatures = StreamFeatures Nothing [] Nothing []
    , streamAddress = Nothing
    , streamFrom = Nothing
    , streamId = Nothing
    , streamLang = Nothing
    , streamJid = Nothing
    , streamConfiguration = def
    }

zeroSource :: Source IO output
zeroSource = liftIO $ do
    debugM "Pontarius.Xmpp" "zeroSource"
    ExL.throwIO XmppOtherFailure

handleToStreamHandle :: Handle -> StreamHandle
handleToStreamHandle h = StreamHandle { streamSend = \d -> catchPush $ BS.hPut h d
                                      , streamReceive = \n -> BS.hGetSome h n
                                      , streamFlush = hFlush h
                                      , streamClose = hClose h
                                      }

createStream :: HostName -> StreamConfiguration -> ErrorT XmppFailure IO (Stream)
createStream realm config = do
    result <- connect realm config
    case result of
        Just hand -> ErrorT $ do
            debugM "Pontarius.Xmpp" "Acquired handle."
            debugM "Pontarius.Xmpp" "Setting NoBuffering mode on handle."
            eSource <- liftIO . bufferSrc $
                         (sourceStreamHandle hand $= logConduit)
                           $= XP.parseBytes def
            let stream = StreamState
                  { streamConnectionState = Plain
                  , streamHandle = hand
                  , streamEventSource = eSource
                  , streamFeatures = StreamFeatures Nothing [] Nothing []
                  , streamAddress = Just $ Text.pack realm
                  , streamFrom = Nothing
                  , streamId = Nothing
                  , streamLang = Nothing
                  , streamJid = Nothing
                  , streamConfiguration = config
                  }
            stream' <- mkStream stream
            return $ Right stream'
        Nothing -> do
            lift $ debugM "Pontarius.Xmpp" "Did not acquire handle."
            throwError TcpConnectionFailure
  where
    logConduit :: Conduit ByteString IO ByteString
    logConduit = CL.mapM $ \d -> do
        debugM "Pontarius.Xmpp" $ "In: " ++ (BSC8.unpack d) ++
            "."
        return d

-- Connects to the provided hostname or IP address. If a hostname is provided, a
-- DNS-SRV lookup is performed (unless `sockAddr' has been specified, in which
-- case that address is used instead). If an A(AAA) record results are
-- encountered, all IP addresses will be tried until a successful connection
-- attempt has been made. Will return the Handle acquired, if any.
connect :: HostName -> StreamConfiguration -> ErrorT XmppFailure IO
           (Maybe StreamHandle)
connect realm config = do
    case connectionDetails config of
        UseHost host port -> lift $ do
            debugM "Pontarius.Xmpp" "Connecting to configured address."
            h <- connectTcp $ [(host, port)]
            case h of
                Nothing -> return Nothing
                Just h' -> do
                    liftIO $ hSetBuffering h' NoBuffering
                    return . Just $ handleToStreamHandle h'
        UseSrv host -> do
            h <- connectSrv (resolvConf config) host
            case h of
                Nothing -> return Nothing
                Just h' -> do
                    liftIO $ hSetBuffering h' NoBuffering
                    return . Just $ handleToStreamHandle h'
        UseRealm -> do
            h <- connectSrv (resolvConf config) realm
            case h of
                Nothing -> return Nothing
                Just h' -> do
                    liftIO $ hSetBuffering h' NoBuffering
                    return . Just $ handleToStreamHandle h'
        UseConnection mkC -> Just <$> mkC

connectSrv :: ResolvConf -> String -> ErrorT XmppFailure IO (Maybe Handle)
connectSrv config host = do
    case checkHostName (Text.pack host) of
        Just host' -> do
            resolvSeed <- lift $ makeResolvSeed config
            lift $ debugM "Pontarius.Xmpp" "Performing SRV lookup..."
            srvRecords <- srvLookup host' resolvSeed
            case srvRecords of
                Nothing -> do
                    lift $ debugM "Pontarius.Xmpp"
                        "No SRV records, using fallback process."
                    lift $ resolvAndConnectTcp resolvSeed (BSC8.pack $ host)
                                               5222
                Just srvRecords' -> do
                    lift $ debugM "Pontarius.Xmpp"
                        "SRV records found, performing A/AAAA lookups."
                    lift $ resolvSrvsAndConnectTcp resolvSeed srvRecords'
        Nothing -> do
                lift $ errorM "Pontarius.Xmpp"
                    "The hostname could not be validated."
                throwError XmppIllegalTcpDetails

-- Connects to a list of addresses and ports. Surpresses any exceptions from
-- connectTcp.
connectTcp :: [(HostName, PortID)] -> IO (Maybe Handle)
connectTcp [] = return Nothing
connectTcp ((address, port):remainder) = do
    result <- Ex.try $ (do
        debugM "Pontarius.Xmpp" $ "Connecting to " ++ address ++ " on port " ++
            (show port) ++ "."
        connectTo address port) :: IO (Either Ex.IOException Handle)
    case result of
        Right handle -> do
            debugM "Pontarius.Xmpp" "Successfully connected to HostName."
            return $ Just handle
        Left _ -> do
            debugM "Pontarius.Xmpp" "Connection to HostName could not be established."
            connectTcp remainder

#if MIN_VERSION_dns(1, 0, 0)
fixDnsResult :: Either e a -> Maybe a
fixDnsResult = either (const Nothing) Just
#else
fixDnsResult :: Maybe a -> Maybe a
fixDnsResult = id
#endif

-- Makes an AAAA query to acquire a IPs, and tries to connect to all of them. If
-- a handle can not be acquired this way, an analogous A query is performed.
-- Surpresses all IO exceptions.
resolvAndConnectTcp :: ResolvSeed -> Domain -> Int -> IO (Maybe Handle)
resolvAndConnectTcp resolvSeed domain port = do
    aaaaResults <- (Ex.try $ rethrowErrorCall $ withResolver resolvSeed $
                        \resolver -> fmap fixDnsResult $ lookupAAAA resolver domain) :: IO (Either Ex.IOException (Maybe [IPv6]))
    handle <- case aaaaResults of
        Right Nothing -> return Nothing
        Right (Just ipv6s) -> connectTcp $
                                  map (\ip -> ( show ip
                                                , PortNumber $ fromIntegral port))
                                      ipv6s
        Left _e -> return Nothing
    case handle of
        Nothing -> do
            aResults <- (Ex.try $ rethrowErrorCall $ withResolver resolvSeed $
                             \resolver -> fmap fixDnsResult $ lookupA resolver domain) :: IO (Either Ex.IOException (Maybe [IPv4]))
            handle' <- case aResults of
                Left  _       -> return Nothing
                Right Nothing -> return Nothing

                Right (Just ipv4s) -> connectTcp $
                                          map (\ip -> (show ip
                                                        , PortNumber
                                                          $ fromIntegral port))
                                              ipv4s
            case handle' of
                Nothing -> return Nothing
                Just handle'' -> return $ Just handle''
        Just handle' -> return $ Just handle'

-- Tries `resolvAndConnectTcp' for every SRV record, stopping if a handle is
-- acquired.
resolvSrvsAndConnectTcp :: ResolvSeed -> [(Domain, Int)] -> IO (Maybe Handle)
resolvSrvsAndConnectTcp _ [] = return Nothing
resolvSrvsAndConnectTcp resolvSeed ((domain, port):remaining) = do
    result <- resolvAndConnectTcp resolvSeed domain port
    case result of
        Just handle -> return $ Just handle
        Nothing -> resolvSrvsAndConnectTcp resolvSeed remaining


-- The DNS functions may make error calls. This function catches any such
-- exceptions and rethrows them as IOExceptions.
rethrowErrorCall :: IO a -> IO a
rethrowErrorCall action = do
    result <- Ex.try action
    case result of
        Right result' -> return result'
        Left (Ex.ErrorCall e) -> Ex.ioError $ userError
                                 $ "rethrowErrorCall: " ++ e

-- Provides a list of A(AAA) names and port numbers upon a successful
-- DNS-SRV request, or `Nothing' if the DNS-SRV request failed.
srvLookup :: Text -> ResolvSeed -> ErrorT XmppFailure IO (Maybe [(Domain, Int)])
srvLookup realm resolvSeed = ErrorT $ do
    result <- Ex.try $ rethrowErrorCall $ withResolver resolvSeed
              $ \resolver -> do
        srvResult <- lookupSRV resolver $ BSC8.pack $ "_xmpp-client._tcp." ++ (Text.unpack realm) ++ "."
        case fixDnsResult srvResult of
            Just [(_, _, _, ".")] -> do
                debugM "Pontarius.Xmpp" $ "\".\" SRV result returned."
                return $ Just []
            Just srvResult' -> do
                debugM "Pontarius.Xmpp" $ "SRV result: " ++ (show srvResult')
                -- Get [(Domain, PortNumber)] of SRV request, if any.
                orderedSrvResult <- orderSrvResult srvResult'
                return $ Just $ Prelude.map (\(_, _, port, domain) -> (domain, port)) orderedSrvResult
            -- The service is not available at this domain.
            -- Sorts the records based on the priority value.
            Nothing -> do
                debugM "Pontarius.Xmpp" "No SRV result returned."
                return Nothing
    case result of
        Right result' -> return $ Right result'
        Left e -> return $ Left $ XmppIOException e
  where
    -- This function orders the SRV result in accordance with RFC
    -- 2782. It sorts the SRV results in order of priority, and then
    -- uses a random process to order the records with the same
    -- priority based on their weight.
    orderSrvResult :: [(Int, Int, Int, Domain)] -> IO [(Int, Int, Int, Domain)]
    orderSrvResult srvResult = do
        -- Order the result set by priority.
        let srvResult' = sortBy (comparing (\(priority, _, _, _) -> priority)) srvResult
        -- Group elements in sublists based on their priority. The
        -- type is `[[(Int, Int, Int, Domain)]]'.
        let srvResult'' = Data.List.groupBy (\(priority, _, _, _) (priority', _, _, _) -> priority == priority') srvResult' :: [[(Int, Int, Int, Domain)]]
        -- For each sublist, put records with a weight of zero first.
        let srvResult''' = Data.List.map (\sublist -> let (a, b) = partition (\(_, weight, _, _) -> weight == 0) sublist in Data.List.concat [a, b]) srvResult''
        -- Order each sublist.
        srvResult'''' <- mapM orderSublist srvResult'''
        -- Concatinated the results.
        return $ Data.List.concat srvResult''''
      where
        orderSublist :: [(Int, Int, Int, Domain)] -> IO [(Int, Int, Int, Domain)]
        orderSublist [] = return []
        orderSublist sublist = do
            -- Compute the running sum, as well as the total sum of
            -- the sublist. Add the running sum to the SRV tuples.
            let (total, sublist') = Data.List.mapAccumL (\total' (priority, weight, port, domain) -> (total' + weight, (priority, weight, port, domain, total' + weight))) 0 sublist
            -- Choose a random number between 0 and the total sum
            -- (inclusive).
            randomNumber <- randomRIO (0, total)
            -- Select the first record with its running sum greater
            -- than or equal to the random number.
            let (beginning, ((priority, weight, port, domain, _):end)) = Data.List.break (\(_, _, _, _, running) -> randomNumber <= running) sublist'
            -- Remove the running total number from the remaining
            -- elements.
            let sublist'' = Data.List.map (\(priority', weight', port', domain', _) -> (priority', weight', port', domain')) (Data.List.concat [beginning, end])
            -- Repeat the ordering procedure on the remaining
            -- elements.
            rest <- orderSublist sublist''
            return $ ((priority, weight, port, domain):rest)

-- | Close the connection and updates the XmppConMonad Stream state. Does
-- not send the stream end tag.
killStream :: Stream -> IO (Either XmppFailure ())
killStream = withStream $ do
    cc <- gets (streamClose . streamHandle)
    err <- wrapIOException cc
    -- (ExL.try cc :: IO (Either ExL.SomeException ()))
    put xmppNoStream{ streamConnectionState = Finished }
    return err

-- Sends an IQ request and waits for the response. If the response ID does not
-- match the outgoing ID, an error is thrown.
pushIQ :: Text
       -> Maybe Jid
       -> IQRequestType
       -> Maybe LangTag
       -> Element
       -> Stream
       -> IO (Either XmppFailure (Either IQError IQResult))
pushIQ iqID to tp lang body stream = runErrorT $ do
    pushing $ pushStanza
        (IQRequestS $ IQRequest iqID Nothing to lang tp body) stream
    res <- lift $ pullStanza stream
    case res of
        Left e -> throwError e
        Right (IQErrorS e) -> return $ Left e
        Right (IQResultS r) -> do
            unless
                (iqID == iqResultID r) $ liftIO $ do
                    liftIO $ errorM "Pontarius.Xmpp" $ "pushIQ: ID mismatch (" ++ (show iqID) ++ " /= " ++ (show $ iqResultID r) ++ ")."
                    liftIO $ ExL.throwIO XmppOtherFailure
                -- TODO: Log: ("In sendIQ' IDs don't match: " ++ show iqID ++
                -- " /= " ++ show (iqResultID r) ++ " .")
            return $ Right r
        _ -> do
                 liftIO $ errorM "Pontarius.Xmpp" $ "pushIQ: Unexpected stanza type."
                 throwError XmppOtherFailure

debugConduit :: (Show o, MonadIO m) => ConduitM o o m b
debugConduit = forever $ do
    s' <- await
    case s' of
        Just s ->  do
            liftIO $ debugM "Pontarius.Xmpp" $ "debugConduit: In: " ++ (show s)
            yield s
        Nothing -> return ()

elements :: R.MonadThrow m => Conduit Event m Element
elements = do
        x <- await
        case x of
            Just (EventBeginElement n as) -> do
                                                 goE n as >>= yield
                                                 elements
            -- This might be an XML error if the end element tag is not
            -- "</stream>". TODO: We might want to check this at a later time
            Just (EventEndElement _) -> lift $ R.monadThrow StreamEnd
            Just (EventContent (ContentText ct)) | Text.all isSpace ct ->
                elements
            Nothing -> return ()
            _ -> lift $ R.monadThrow $ InvalidXmppXml $ "not an element: " ++ show x
  where
    many' f =
        go id
      where
        go front = do
            x <- f
            case x of
                Left l -> return $ (l, front [])
                Right r -> go (front . (:) r)
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

withStream :: StateT StreamState IO a -> Stream -> IO a
withStream action (Stream stream) = Ex.bracketOnError
                                         (atomically $ takeTMVar stream )
                                         (atomically . putTMVar stream)
                                         (\s -> do
                                               (r, s') <- runStateT action s
                                               atomically $ putTMVar stream s'
                                               return r
                                         )

-- nonblocking version. Changes to the connection are ignored!
withStream' :: StateT StreamState IO a -> Stream -> IO a
withStream' action (Stream stream) = do
    stream_ <- atomically $ readTMVar stream
    (r, _) <- runStateT action stream_
    return r


mkStream :: StreamState -> IO Stream
mkStream con = Stream `fmap` atomically (newTMVar con)

-- "Borrowed" from base-4.4 for compatibility with GHC 7.0.1.
tryIOError :: IO a -> IO (Either IOError a)
tryIOError f = Ex.catch (Right <$> f) (return . Left)
