{-# OPTIONS_HADDOCK hide #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.Xmpp.Connection where

import           Control.Applicative((<$>))
import           Control.Concurrent (forkIO, threadDelay)
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
import           Network.Xmpp.Pickle

import           System.IO

import           Text.XML.Stream.Elements
import           Text.XML.Stream.Parse as XP
import           Text.XML.Unresolved(InvalidEventStream(..))

import           System.Log.Logger
import           Data.ByteString.Base64

-- Enable/disable debug output
-- This will dump all incoming and outgoing network taffic to the console,
-- prefixed with "in: " and "out: " respectively
debug :: Bool
debug = False

pushElement :: Element -> StateT Connection_ IO Bool
pushElement x = do
    send <- gets (cSend . cHand)
    liftIO . send $ renderElement x

-- | Encode and send stanza
pushStanza :: Stanza -> Connection -> IO Bool
pushStanza s = withConnection' . pushElement $ pickleElem xpStanza s

-- XML documents and XMPP streams SHOULD be preceeded by an XML declaration.
-- UTF-8 is the only supported XMPP encoding. The standalone document
-- declaration (matching "SDDecl" in the XML standard) MUST NOT be included in
-- XMPP streams. RFC 6120 defines XMPP only in terms of XML 1.0.
pushXmlDecl :: StateT Connection_ IO Bool
pushXmlDecl = do
    con <- gets cHand
    liftIO $ (cSend con) "<?xml version='1.0' encoding='UTF-8' ?>"

pushOpenElement :: Element -> StateT Connection_ IO Bool
pushOpenElement e = do
    sink <- gets (cSend . cHand )
    liftIO . sink $ renderOpenElement e

-- `Connect-and-resumes' the given sink to the connection source, and pulls a
-- `b' value.
runEventsSink :: Sink Event IO b -> StateT Connection_ IO b
runEventsSink snk = do
    source <- gets cEventSource
    (src', r) <- lift $ source $$++ snk
    modify (\s -> s{cEventSource = src'})
    return r

pullElement :: StateT Connection_ IO Element
pullElement = do
    Ex.catches (do
        e <- runEventsSink (elements =$ await)
        case e of
            Nothing -> liftIO $ Ex.throwIO StreamOtherFailure
            Just r -> return r
        )
        [ Ex.Handler (\StreamEnd -> Ex.throwIO StreamEndFailure)
        , Ex.Handler (\(InvalidXmppXml s) -- Invalid XML `Event' encountered, or missing element close tag
                     -> liftIO . Ex.throwIO $ StreamOtherFailure) -- TODO: Log: s
        , Ex.Handler $ \(e :: InvalidEventStream) -- xml-conduit exception
                     -> liftIO . Ex.throwIO $ StreamOtherFailure -- TODO: Log: (show e)
        ]

-- Pulls an element and unpickles it.
pullUnpickle :: PU [Node] a -> StateT Connection_ IO a
pullUnpickle p = do
    res <- unpickleElem p <$> pullElement
    case res of
        Left e -> liftIO $ Ex.throwIO e
        Right r -> return r

-- | Pulls a stanza (or stream error) from the stream. Throws an error on a stream
-- error.
pullStanza :: Connection -> IO Stanza
pullStanza = withConnection' $ do
    res <- pullUnpickle xpStreamStanza
    case res of
        Left e -> liftIO . Ex.throwIO $ StreamErrorFailure e
        Right r -> return r

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

-- -- Connection_ state used when there is no connection.
xmppNoConnection :: Connection_
xmppNoConnection = Connection_
               { cHand            = Hand { cSend = \_ -> return False
                                         , cRecv = \_ -> Ex.throwIO
                                                         $ StreamOtherFailure
                                         , cFlush = return ()
                                         , cClose = return ()
                                         }
               , cEventSource     = DCI.ResumableSource zeroSource (return ())
               , sFeatures        = SF Nothing [] []
               , sConnectionState = ConnectionClosed
               , sHostname        = Nothing
               , sJid             = Nothing
               , sStreamLang      = Nothing
               , sStreamId        = Nothing
               , sPreferredLang   = Nothing
               , sToJid           = Nothing
               , sJidWhenPlain    = False
               , sFrom            = Nothing
               }
  where
    zeroSource :: Source IO output
    zeroSource = liftIO . Ex.throwIO $ StreamOtherFailure

-- Connects to the given hostname on port 5222 (TODO: Make this dynamic) and
-- updates the XmppConMonad Connection_ state.
connectTcpRaw :: HostName -> PortID -> Text -> IO Connection
connectTcpRaw host port hostname = do
    let PortNumber portNumber = port
    debugM "Pontarius.Xmpp" $ "Connecting to " ++ host ++ " on port " ++
        (show portNumber) ++ " through the realm " ++ (T.unpack hostname) ++ "."
    h <- connectTo host port
    debugM "Pontarius.Xmpp" "Setting NoBuffering mode on handle."
    hSetBuffering h NoBuffering
    let eSource = DCI.ResumableSource ((sourceHandle h $= logConduit) $= XP.parseBytes def)
                                      (return ())
    let hand = Hand { cSend = \d -> do
                         let d64 = encode d
                         debugM "Pontarius.Xmpp" $ "Sending TCP data: " ++
                             (BSC8.unpack d64) ++ "."
                         catchPush $ BS.hPut h d
                    , cRecv = \n -> do
                         d <- BS.hGetSome h n
                         let d64 = encode d
                         debugM "Pontarius.Xmpp" $ "Received TCP data: " ++
                             (BSC8.unpack d64) ++ "."
                         return d
                    , cFlush = hFlush h
                    , cClose = hClose h
                    }
    let con = Connection_
            { cHand            = hand
            , cEventSource     = eSource
            , sFeatures        = (SF Nothing [] [])
            , sConnectionState = ConnectionPlain
            , sHostname        = (Just hostname)
            , sJid             = Nothing
            , sPreferredLang   = Nothing -- TODO: Allow user to set
            , sStreamLang      = Nothing
            , sStreamId        = Nothing
            , sToJid           = Nothing -- TODO: Allow user to set
            , sJidWhenPlain    = False -- TODO: Allow user to set
            , sFrom            = Nothing
            }
    mkConnection con
  where
    logConduit :: Conduit ByteString IO ByteString
    logConduit = CL.mapM $ \d -> do
        let d64 = encode d
        debugM "Pontarius.Xmpp" $ "Received TCP data: " ++ (BSC8.unpack d64) ++
            "."
        return d


-- Closes the connection and updates the XmppConMonad Connection_ state.
killConnection :: Connection -> IO (Either Ex.SomeException ())
killConnection = withConnection $ do
    cc <- gets (cClose . cHand)
    err <- liftIO $ (Ex.try cc :: IO (Either Ex.SomeException ()))
    put xmppNoConnection
    return err

-- Sends an IQ request and waits for the response. If the response ID does not
-- match the outgoing ID, an error is thrown.
pushIQ' :: StanzaId
            -> Maybe Jid
            -> IQRequestType
            -> Maybe LangTag
            -> Element
            -> Connection
            -> IO (Either IQError IQResult)
pushIQ' iqID to tp lang body con = do
    pushStanza (IQRequestS $ IQRequest iqID Nothing to lang tp body) con
    res <- pullStanza con
    case res of
        IQErrorS e -> return $ Left e
        IQResultS r -> do
            unless
                (iqID == iqResultID r) . liftIO . Ex.throwIO $
                    StreamOtherFailure
                -- TODO: Log: ("In sendIQ' IDs don't match: " ++ show iqID ++
                -- " /= " ++ show (iqResultID r) ++ " .")
            return $ Right r
        _ -> liftIO $ Ex.throwIO StreamOtherFailure
             -- TODO: Log: "sendIQ': unexpected stanza type "

-- | Send "</stream:stream>" and wait for the server to finish processing and to
-- close the connection. Any remaining elements from the server and whether or
-- not we received a </stream:stream> element from the server is returned.
closeStreams :: Connection -> IO ([Element], Bool)
closeStreams = withConnection $ do
    send <- gets (cSend . cHand)
    cc <- gets (cClose . cHand)
    liftIO $ send "</stream:stream>"
    void $ liftIO $ forkIO $ do
        threadDelay 3000000
        (Ex.try cc) :: IO (Either Ex.SomeException ())
        return ()
    collectElems []
  where
    -- Pulls elements from the stream until the stream ends, or an error is
    -- raised.
    collectElems :: [Element] -> StateT Connection_ IO ([Element], Bool)
    collectElems es = do
        result <- Ex.try pullElement
        case result of
            Left StreamEndFailure -> return (es, True)
            Left _ -> return (es, False)
            Right e -> collectElems (e:es)

debugConduit :: Pipe l ByteString ByteString u IO b
debugConduit = forever $ do
    s' <- await
    case s' of
        Just s ->  do
            liftIO $ BS.putStrLn (BS.append "in: " s)
            yield s
        Nothing -> return ()
