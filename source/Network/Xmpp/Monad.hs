{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.Xmpp.Monad where

import           Control.Applicative((<$>))
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class
--import Control.Monad.Trans.Resource
import qualified Control.Exception.Lifted as Ex
import qualified GHC.IO.Exception as GIE
import           Control.Monad.State.Strict

import           Data.ByteString as BS
import           Data.Conduit
import qualified Data.Conduit.List as CL
import           Data.Conduit.BufferedSource
import           Data.Conduit.Binary as CB
import           Data.Text(Text)
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

pushElement :: Element -> XmppConMonad Bool
pushElement x = do
    sink <- gets sConPushBS
    liftIO . sink $ renderElement x

pushStanza :: Stanza -> XmppConMonad Bool
pushStanza = pushElement . pickleElem xpStanza

-- XML documents and XMPP streams SHOULD be preceeded by an XML declaration.
-- UTF-8 is the only supported XMPP encoding. The standalone document
-- declaration (matching "SDDecl" in the XML standard) MUST NOT be included in
-- XMPP streams. RFC 6120 defines XMPP only in terms of XML 1.0.
pushXmlDecl :: XmppConMonad Bool
pushXmlDecl = do
    sink <- gets sConPushBS
    liftIO $ sink "<?xml version='1.0' encoding='UTF-8' ?>"

pushOpenElement :: Element -> XmppConMonad Bool
pushOpenElement e = do
    sink <- gets sConPushBS
    liftIO . sink $ renderOpenElement e

-- `Connect-and-resumes' the given sink to the connection source, and pulls a
-- `b' value.
pullToSink :: Sink Event IO b -> XmppConMonad b
pullToSink snk = do
    source <- gets sConSrc
    (_, r) <- lift $ source $$+ snk
    return r

pullElement :: XmppConMonad Element
pullElement = do
    Ex.catch (do
        e <- pullToSink (elements =$ CL.head)
        case e of
            Nothing -> liftIO $ Ex.throwIO StreamConnectionError
            Just r -> return r
        )
        (\(InvalidEventStream s) -> liftIO . Ex.throwIO $ StreamXMLError s)

-- Pulls an element and unpickles it.
pullPickle :: PU [Node] a -> XmppConMonad a
pullPickle p = do
    res <- unpickleElem p <$> pullElement
    case res of
        Left e -> liftIO . Ex.throwIO $ StreamXMLError e
        Right r -> return r

-- Pulls a stanza from the stream. Throws an error on failure.
pullStanza  :: XmppConMonad Stanza
pullStanza = do
    res <- pullPickle xpStreamStanza
    case res of
        Left e -> liftIO . Ex.throwIO $ StreamError e
        Right r -> return r

-- Performs the given IO operation, catches any errors and re-throws everything
-- except the `ResourceVanished' error.
catchPush :: IO () -> IO Bool
catchPush p = Ex.catch
    (p >> return True)
    (\e -> case GIE.ioe_type e of
         GIE.ResourceVanished -> return False
         _ -> Ex.throwIO e
    )

-- XmppConnection state used when there is no connection.
xmppNoConnection :: XmppConnection
xmppNoConnection = XmppConnection
               { sConSrc          = zeroSource
               , sRawSrc          = zeroSource
               , sConPushBS       = \_ -> return False -- Nothing has been sent.
               , sConHandle       = Nothing
               , sFeatures        = SF Nothing [] []
               , sConnectionState = XmppConnectionClosed
               , sHostname        = Nothing
               , sJid             = Nothing
               , sCloseConnection = return ()
               }
  where
    zeroSource :: Source IO output
    zeroSource = liftIO . Ex.throwIO $ StreamConnectionError

-- Connects to the given hostname on port 5222 (TODO: Make this dynamic) and
-- updates the XmppConMonad XmppConnection state.
xmppRawConnect :: HostName -> Text -> XmppConMonad ()
xmppRawConnect host hostname = do
    con <- liftIO $ do
        con <- connectTo host (PortNumber 5222)
        hSetBuffering con NoBuffering
        return con
    let raw = sourceHandle con
    src <- liftIO . bufferSource $ raw $= XP.parseBytes def
    let st = XmppConnection
            { sConSrc          = src
            , sRawSrc          = raw
            , sConPushBS       = (catchPush . BS.hPut con)
            , sConHandle       = (Just con)
            , sFeatures        = (SF Nothing [] [])
            , sConnectionState = XmppConnectionPlain
            , sHostname        = (Just hostname)
            , sJid             = Nothing
            , sCloseConnection = (hClose con)
            }
    put st

-- Execute a XmppConMonad computation.
xmppNewSession :: XmppConMonad a -> IO (a, XmppConnection)
xmppNewSession action = runStateT action xmppNoConnection

-- Closes the connection and updates the XmppConMonad XmppConnection state.
xmppKillConnection :: XmppConMonad ()
xmppKillConnection = do
    cc <- gets sCloseConnection
    void . liftIO $ (Ex.try cc :: IO (Either Ex.SomeException ()))
    put xmppNoConnection

-- Sends an IQ request and waits for the response. If the response ID does not
-- match the outgoing ID, an error is thrown.
xmppSendIQ' :: StanzaId
            -> Maybe Jid
            -> IQRequestType
            -> Maybe LangTag
            -> Element
            -> XmppConMonad (Either IQError IQResult)
xmppSendIQ' iqID to tp lang body = do
    pushStanza . IQRequestS $ IQRequest iqID Nothing to lang tp body
    res <- pullPickle $ xpEither xpIQError xpIQResult
    case res of
        Left e -> return $ Left e
        Right iq' -> do
            unless
                (iqID == iqResultID iq') . liftIO . Ex.throwIO $
                    StreamXMLError
                ("In xmppSendIQ' IDs don't match: " ++ show iqID ++ " /= " ++
                    show (iqResultID iq') ++ " .")
            return $ Right iq'