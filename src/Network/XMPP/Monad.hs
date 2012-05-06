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

pushN :: Element -> XMPPConMonad Bool
pushN x = do
  sink <- gets sConPushBS
  liftIO . sink $ renderElement x

push :: Stanza -> XMPPConMonad Bool
push = pushN . pickleElem xpStanza

pushOpen :: Element -> XMPPConMonad Bool
pushOpen e = do
  sink <- gets sConPushBS
  liftIO . sink $ renderOpenElement e

pullSink :: Sink Event IO b -> XMPPConMonad b
pullSink snk = do
  source <- gets sConSrc
  (_, r) <- lift $ source $$+ snk
  return r

pullElement :: XMPPConMonad Element
pullElement = do
    Ex.catch (do
        e <- pullSink (elements =$ CL.head)
        case e of
            Nothing -> liftIO $ Ex.throwIO StreamConnectionError
            Just r -> return r
        )
        (\(InvalidEventStream s) -> liftIO . Ex.throwIO $ StreamXMLError s)


pullPickle :: PU [Node] a -> XMPPConMonad a
pullPickle p = do
    res <- unpickleElem p <$> pullElement
    case res of
        Left e -> liftIO . Ex.throwIO $ StreamXMLError e
        Right r -> return r

pullStanza  :: XMPPConMonad Stanza
pullStanza = do
    res <- pullPickle xpStreamEntity
    case res of
        Left e -> liftIO . Ex.throwIO $ StreamError e
        Right r -> return r

catchPush p = Ex.catch (p >> return True)
                       (\e -> case GIE.ioe_type e of
                                   GIE.ResourceVanished -> return False
                                   _ -> Ex.throwIO e
                       )

zeroSource :: Source IO output
zeroSource = liftIO . Ex.throwIO $ StreamConnectionError

xmppNoConnection :: XmppConnection
xmppNoConnection = XmppConnection
               { sConSrc    = zeroSource
               , sRawSrc    = zeroSource
               , sConPushBS = \_ -> return False
               , sConHandle = Nothing
               , sFeatures  = SF Nothing [] []
               , sConnectionState = XmppConnectionClosed
               , sHostname  = Nothing
               , sUsername  = Nothing
               , sResource  = Nothing
               , sCloseConnection = return ()
               }

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
             (raw)
             (catchPush . BS.hPut con)
             (Just con)
             (SF Nothing [] [])
             XmppConnectionPlain
             (Just hostname)
             uname
             Nothing
             (hClose con)
  put st

xmppNewSession :: XMPPConMonad a -> IO (a, XmppConnection)
xmppNewSession action = do
  runStateT action xmppNoConnection

xmppKillConnection :: XMPPConMonad ()
xmppKillConnection = do
    cc <- gets sCloseConnection
    void . liftIO $ (Ex.try cc :: IO (Either Ex.SomeException ()))
    put xmppNoConnection

xmppSendIQ' :: StanzaId -> Maybe JID -> IQRequestType
            -> Maybe LangTag -> Element
            -> XMPPConMonad (Either IQError IQResult)
xmppSendIQ' iqID to tp lang body = do
    push . IQRequestS $ IQRequest iqID Nothing to lang tp body
    res <- pullPickle $ xpEither xpIQError xpIQResult
    case res of
        Left e -> return $ Left e
        Right iq' -> do
            unless (iqID == iqResultID iq') . liftIO . Ex.throwIO $
                StreamXMLError
              ("In xmppSendIQ' IDs don't match: " ++ show iqID ++
              " /= " ++ show (iqResultID iq') ++ " .")
            return $ Right iq'

