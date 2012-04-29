{-# LANGUAGE OverloadedStrings #-}

module Network.XMPP.Monad where

import           Control.Applicative((<$>))
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class
--import Control.Monad.Trans.Resource
import           Control.Concurrent
import qualified Control.Exception as Ex
import           Control.Monad.State.Strict

import           Data.ByteString as BS
import           Data.Conduit
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

pushN :: Element -> XMPPConMonad ()
pushN x = do
  sink <- gets sConPushBS
  liftIO . sink $ renderElement x

push :: Stanza -> XMPPConMonad ()
push = pushN . pickleElem xpStanza

pushOpen :: Element -> XMPPConMonad ()
pushOpen e = do
  sink <- gets sConPushBS
  liftIO . sink $ renderOpenElement e
  return ()

pullSink :: Sink Event IO b -> XMPPConMonad b
pullSink snk = do
  source <- gets sConSrc
  (_, r) <- lift $ source $$+ snk
  return r

pullElement :: XMPPConMonad Element
pullElement = pullSink elementFromEvents

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

xmppFromHandle :: Handle
               -> Text
               -> Text
               -> Maybe Text
               -> XMPPConMonad a
               -> IO (a, XMPPConState)
xmppFromHandle handle hostname username res f = do
  liftIO $ hSetBuffering handle NoBuffering
  let raw = sourceHandle handle
  let src = raw $= XP.parseBytes def
  let st = XMPPConState
             src
             (raw)
             (BS.hPut handle)
             (Just handle)
             (SF Nothing [] [])
             False
             (Just hostname)
             (Just username)
             res
             (hClose handle)
  runStateT f st

zeroSource :: Source IO output
zeroSource = liftIO . forever $ threadDelay 10000000

xmppZeroConState :: XMPPConState
xmppZeroConState = XMPPConState
               { sConSrc    = zeroSource
               , sRawSrc    = zeroSource
               , sConPushBS = (\_ -> return ())
               , sConHandle = Nothing
               , sFeatures  = SF Nothing [] []
               , sHaveTLS   = False
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
  let st = XMPPConState
             src
             (raw)
             (BS.hPut con)
             (Just con)
             (SF Nothing [] [])
             False
             (Just hostname)
             uname
             Nothing
             (hClose con)
  put st

xmppNewSession :: XMPPConMonad a -> IO (a, XMPPConState)
xmppNewSession action = do
  runStateT action xmppZeroConState

xmppKillConnection :: XMPPConMonad ()
xmppKillConnection = do
    cc <- gets sCloseConnection
    liftIO cc
    put xmppZeroConState

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

