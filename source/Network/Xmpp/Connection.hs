{-# OPTIONS_HADDOCK hide #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.Xmpp.Connection where

import           Control.Applicative((<$>))
import           Control.Concurrent (forkIO, threadDelay)
import           System.IO.Error (tryIOError)
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class
--import Control.Monad.Trans.Resource
import qualified Control.Exception.Lifted as Ex
import qualified GHC.IO.Exception as GIE
import           Control.Monad.State.Strict

import           Data.ByteString as BS
import           Data.ByteString.Char8 as BSC8
import           Data.Conduit
import           Data.Conduit.Binary as CB
import           Data.Conduit.Internal as DCI
import qualified Data.Conduit.List as CL
import           Data.IORef
import           Data.Text(Text)
import qualified Data.Text as T
import           Data.XML.Pickle
import           Data.XML.Types

import           Network
import           Network.Xmpp.Types
import           Network.Xmpp.Marshal

import           System.IO

import           Text.Xml.Stream.Elements
import           Text.XML.Stream.Parse as XP
import           Text.XML.Unresolved(InvalidEventStream(..))

import           System.Log.Logger
import           Data.ByteString.Base64

import           Control.Concurrent.STM.TMVar
import           Control.Monad.Error

-- Enable/disable debug output
-- This will dump all incoming and outgoing network taffic to the console,
-- prefixed with "in: " and "out: " respectively
debug :: Bool
debug = False

-- TODO: Can the TLS send/recv functions throw something other than an IO error?

wrapIOException :: IO a -> StateT Connection IO (Either XmppFailure a)
wrapIOException action = do
    r <- liftIO $ tryIOError action
    case r of
        Right b -> return $ Right b
        Left e -> return $ Left $ XmppIOException e

pushElement :: Element -> StateT Connection IO (Either XmppFailure Bool)
pushElement x = do
    send <- gets (cSend . cHandle)
    wrapIOException $ send $ renderElement x

-- | Encode and send stanza
pushStanza :: Stanza -> TMVar Connection -> IO (Either XmppFailure Bool)
pushStanza s = withConnection' . pushElement $ pickleElem xpStanza s

-- XML documents and XMPP streams SHOULD be preceeded by an XML declaration.
-- UTF-8 is the only supported XMPP encoding. The standalone document
-- declaration (matching "SDDecl" in the XML standard) MUST NOT be included in
-- XMPP streams. RFC 6120 defines XMPP only in terms of XML 1.0.
pushXmlDecl :: StateT Connection IO (Either XmppFailure Bool)
pushXmlDecl = do
    con <- gets cHandle
    wrapIOException $ (cSend con) "<?xml version=\"1.0\" encoding=\"UTF-8\" ?>"

pushOpenElement :: Element -> StateT Connection IO (Either XmppFailure Bool)
pushOpenElement e = do
    sink <- gets (cSend . cHandle)
    wrapIOException $ sink $ renderOpenElement e

-- `Connect-and-resumes' the given sink to the connection source, and pulls a
-- `b' value.
runEventsSink :: Sink Event IO b -> StateT Connection IO (Either XmppFailure b)
runEventsSink snk = do -- TODO: Wrap exceptions?
    source <- gets cEventSource
    (src', r) <- lift $ source $$++ snk
    modify (\s -> s{cEventSource = src'})
    return $ Right r

pullElement :: StateT Connection IO (Either XmppFailure Element)
pullElement = do
    Ex.catches (do
        e <- runEventsSink (elements =$ await)
        case e of
            Left f -> return $ Left f
            Right Nothing -> return $ Left XmppOtherFailure -- TODO
            Right (Just r) -> return $ Right r
        )
        [ Ex.Handler (\StreamEnd -> return $ Left StreamEndFailure)
        , Ex.Handler (\(InvalidXmppXml s) -- Invalid XML `Event' encountered, or missing element close tag
                     -> return $ Left XmppOtherFailure) -- TODO: Log: s
        , Ex.Handler $ \(e :: InvalidEventStream) -- xml-conduit exception
                     -> return $ Left XmppOtherFailure -- TODO: Log: (show e)
        ]

-- Pulls an element and unpickles it.
pullUnpickle :: PU [Node] a -> StateT Connection IO (Either XmppFailure a)
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
pullStanza :: TMVar Connection -> IO (Either XmppFailure Stanza)
pullStanza = withConnection' $ do
    res <- pullUnpickle xpStreamStanza
    case res of
        Left e -> return $ Left e
        Right (Left e) -> return $ Left $ StreamErrorFailure e
        Right (Right r) -> return $ Right r

-- Performs the given IO operation, catches any errors and re-throws everything
-- except 'ResourceVanished' and IllegalOperation, in which case it will return False instead
catchPush :: IO () -> IO Bool
catchPush p = Ex.catch
    (p >> return True)
    (\e -> case GIE.ioe_type e of
         GIE.ResourceVanished -> return False
         GIE.IllegalOperation -> return False
         _ -> Ex.throwIO e
    )

-- Connection state used when there is no connection.
xmppNoConnection :: Connection
xmppNoConnection = Connection
               { cHandle = ConnectionHandle { cSend = \_ -> return False
                                            , cRecv = \_ -> Ex.throwIO
                                                            XmppOtherFailure
                                            , cFlush = return ()
                                            , cClose = return ()
                                            }
               , cEventSource = DCI.ResumableSource zeroSource (return ())
               , cFeatures = SF Nothing [] []
               , cState = ConnectionClosed
               , cHostName = Nothing
               , cJid = Nothing
               , cStreamLang = Nothing
               , cStreamId = Nothing
               , cPreferredLang = Nothing
               , cToJid = Nothing
               , cJidWhenPlain = False
               , cFrom = Nothing
               }
  where
    zeroSource :: Source IO output
    zeroSource = liftIO . Ex.throwIO $ XmppOtherFailure

connectTcp :: HostName -> PortID -> Text -> IO (Either XmppFailure (TMVar Connection))
connectTcp host port hostname = do
    let PortNumber portNumber = port
    debugM "Pontarius.Xmpp" $ "Connecting to " ++ host ++ " on port " ++
        (show portNumber) ++ " through the realm " ++ (T.unpack hostname) ++ "."
    h <- connectTo host port
    debugM "Pontarius.Xmpp" "Setting NoBuffering mode on handle."
    hSetBuffering h NoBuffering
    let eSource = DCI.ResumableSource
                  ((sourceHandle h $= logConduit) $= XP.parseBytes def)
                  (return ())
    let hand = ConnectionHandle { cSend = \d -> do
                                     let d64 = encode d
                                     debugM "Pontarius.Xmpp" $
                                       "Sending TCP data: " ++ (BSC8.unpack d64)
                                       ++ "."
                                     catchPush $ BS.hPut h d
                                , cRecv = \n -> do
                                     d <- BS.hGetSome h n
                                     let d64 = encode d
                                     debugM "Pontarius.Xmpp" $
                                       "Received TCP data: " ++
                                       (BSC8.unpack d64) ++ "."
                                     return d
                                , cFlush = hFlush h
                                , cClose = hClose h
                                }
    let con = Connection
            { cHandle = hand
            , cEventSource = eSource
            , cFeatures = (SF Nothing [] [])
            , cState = ConnectionPlain
            , cHostName = (Just hostname)
            , cJid = Nothing
            , cPreferredLang = Nothing -- TODO: Allow user to set
            , cStreamLang = Nothing
            , cStreamId = Nothing
            , cToJid = Nothing -- TODO: Allow user to set
            , cJidWhenPlain = False -- TODO: Allow user to set
            , cFrom = Nothing
            }
    con' <- mkConnection con
    return $ Right con'
  where
    logConduit :: Conduit ByteString IO ByteString
    logConduit = CL.mapM $ \d -> do
        let d64 = encode d
        debugM "Pontarius.Xmpp" $ "Received TCP data: " ++ (BSC8.unpack d64) ++
            "."
        return d


-- Closes the connection and updates the XmppConMonad Connection state.
-- killConnection :: TMVar Connection -> IO (Either Ex.SomeException ())
killConnection :: TMVar Connection -> IO (Either XmppFailure ())
killConnection = withConnection $ do
    cc <- gets (cClose . cHandle)
    err <- wrapIOException cc
    -- (Ex.try cc :: IO (Either Ex.SomeException ()))
    put xmppNoConnection
    return err

-- Sends an IQ request and waits for the response. If the response ID does not
-- match the outgoing ID, an error is thrown.
pushIQ' :: StanzaId
            -> Maybe Jid
            -> IQRequestType
            -> Maybe LangTag
            -> Element
            -> TMVar Connection
            -> IO (Either XmppFailure (Either IQError IQResult))
pushIQ' iqID to tp lang body con = do
    pushStanza (IQRequestS $ IQRequest iqID Nothing to lang tp body) con
    res <- pullStanza con
    case res of
        Left e -> return $ Left e
        Right (IQErrorS e) -> return $ Right $ Left e
        Right (IQResultS r) -> do
            unless
                (iqID == iqResultID r) . liftIO . Ex.throwIO $
                    XmppOtherFailure
                -- TODO: Log: ("In sendIQ' IDs don't match: " ++ show iqID ++
                -- " /= " ++ show (iqResultID r) ++ " .")
            return $ Right $ Right r
        _ -> return $ Left XmppOtherFailure
             -- TODO: Log: "sendIQ': unexpected stanza type "

-- | Send "</stream:stream>" and wait for the server to finish processing and to
-- close the connection. Any remaining elements from the server are returned.
-- Surpresses StreamEndFailure exceptions, but may throw a StreamCloseError.
closeStreams :: TMVar Connection -> IO (Either XmppFailure [Element])
closeStreams = withConnection $ do
    send <- gets (cSend . cHandle)
    cc <- gets (cClose . cHandle)
    liftIO $ send "</stream:stream>"
    void $ liftIO $ forkIO $ do
        threadDelay 3000000 -- TODO: Configurable value
        (Ex.try cc) :: IO (Either Ex.SomeException ())
        return ()
    collectElems []
  where
    -- Pulls elements from the stream until the stream ends, or an error is
    -- raised.
    collectElems :: [Element] -> StateT Connection IO (Either XmppFailure [Element])
    collectElems es = do
        result <- pullElement
        case result of
            Left StreamEndFailure -> return $ Right es
            Left e -> return $ Left $ StreamCloseError (es, e)
            Right e -> collectElems (e:es)

debugConduit :: Pipe l ByteString ByteString u IO b
debugConduit = forever $ do
    s' <- await
    case s' of
        Just s ->  do
            liftIO $ BS.putStrLn (BS.append "in: " s)
            yield s
        Nothing -> return ()
