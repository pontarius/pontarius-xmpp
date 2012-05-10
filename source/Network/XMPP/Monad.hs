{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.XMPP.Monad where

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
import           Network.XMPP.Types
import           Network.XMPP.Marshal
import           Network.XMPP.Pickle

import           System.IO

import           Text.XML.Stream.Elements
import           Text.XML.Stream.Parse as XP
import           Text.XML.Unresolved(InvalidEventStream(..))

pushElement :: Element -> XMPPConMonad Bool
pushElement x = do
    sink <- gets sConPushBS
    liftIO . sink $ renderElement x

pushStanza :: Stanza -> XMPPConMonad Bool
pushStanza = pushElement . pickleElem xpStanza

pushOpenElement :: Element -> XMPPConMonad Bool
pushOpenElement e = do
    sink <- gets sConPushBS
    liftIO . sink $ renderOpenElement e

-- `Connect-and-resumes' the given sink to the connection source, and pulls a
-- `b' value.
pullToSink :: Sink Event IO b -> XMPPConMonad b
pullToSink snk = do
    source <- gets sConSrc
    (_, r) <- lift $ source $$+ snk
    return r

pullElement :: XMPPConMonad Element
pullElement = do
    Ex.catch (do
        e <- pullToSink (elements =$ CL.head)
        case e of
            Nothing -> liftIO $ Ex.throwIO StreamConnectionError
            Just r -> return r
        )
        (\(InvalidEventStream s) -> liftIO . Ex.throwIO $ StreamXMLError s)

-- Pulls an element and unpickles it.
pullPickle :: PU [Node] a -> XMPPConMonad a
pullPickle p = do
    res <- unpickleElem p <$> pullElement
    case res of
        Left e -> liftIO . Ex.throwIO $ StreamXMLError e
        Right r -> return r

-- Pulls a stanza from the stream. Throws an error on failure.
pullStanza  :: XMPPConMonad Stanza
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
               { sConSrc    = zeroSource
               , sRawSrc    = zeroSource
               , sConPushBS = \_ -> return False -- Nothing has been sent.
               , sConHandle = Nothing
               , sFeatures  = SF Nothing [] []
               , sConnectionState = XmppConnectionClosed
               , sHostname  = Nothing
               , sUsername  = Nothing
               , sResource  = Nothing
               , sCloseConnection = return ()
               }
  where
    zeroSource :: Source IO output
    zeroSource = liftIO . Ex.throwIO $ StreamConnectionError

-- Connects to the given hostname on port 5222 (TODO: Make this dynamic) and
-- updates the XMPPConMonad XmppConnection state.
xmppRawConnect :: HostName -> Text -> XMPPConMonad ()
xmppRawConnect host hostname = do
    uname <- gets sUsername
    con <- liftIO $ do
        con <- connectTo host (PortNumber 5222)
        hSetBuffering con NoBuffering
        return con
    let raw = sourceHandle con
    src <- liftIO . bufferSource $ raw $= XP.parseBytes def
    let st = XmppConnection
            src
            raw
            (catchPush . BS.hPut con)
            (Just con)
            (SF Nothing [] [])
            XmppConnectionPlain
            (Just hostname)
            uname
            Nothing
            (hClose con)
    put st

-- Execute a XMPPConMonad computation.
xmppNewSession :: XMPPConMonad a -> IO (a, XmppConnection)
xmppNewSession action = runStateT action xmppNoConnection

-- Closes the connection and updates the XMPPConMonad XmppConnection state.
xmppKillConnection :: XMPPConMonad ()
xmppKillConnection = do
    cc <- gets sCloseConnection
    void . liftIO $ (Ex.try cc :: IO (Either Ex.SomeException ()))
    put xmppNoConnection

-- Sends an IQ request and waits for the response. If the response ID does not
-- match the outgoing ID, an error is thrown.
xmppSendIQ' :: StanzaId
            -> Maybe JID
            -> IQRequestType
            -> Maybe LangTag
            -> Element
            -> XMPPConMonad (Either IQError IQResult)
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