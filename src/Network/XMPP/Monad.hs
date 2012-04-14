{-# LANGUAGE OverloadedStrings #-}

module Network.XMPP.Monad where

import Control.Applicative((<$>))
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
--import Control.Monad.Trans.Resource
import Control.Monad.Trans.State

import Data.ByteString as BS
import Data.Conduit
import Data.Conduit.Binary as CB
import Data.Conduit.List as CL
import Data.Text(Text)
import Data.XML.Pickle
import Data.XML.Types

import Network
import Network.XMPP.Types
import Network.XMPP.Marshal
import Network.XMPP.Pickle

import System.IO

import Text.XML.Stream.Elements
import Text.XML.Stream.Parse as XP
import Text.XML.Stream.Render as XR


pushN :: Element -> XMPPConMonad ()
pushN x = do
  sink <- gets sConPush
  lift . sink $ elementToEvents x

push :: Stanza -> XMPPConMonad ()
push = pushN . pickleElem stanzaP

pushOpen :: Element -> XMPPConMonad ()
pushOpen e = do
  sink <- gets sConPush
  lift . sink $ openElementToEvents e
  return ()

pulls :: Sink Event IO b -> XMPPConMonad b
pulls snk = do
  source <- gets sConSrc
  (src', r) <- lift $ source $$+ snk
  modify $ (\s -> s {sConSrc = src'})
  return r

pullE :: XMPPConMonad Element
pullE = pulls elementFromEvents

pullPickle :: PU [Node] a -> XMPPConMonad a
pullPickle p = unpickleElem p <$> pullE

pull :: XMPPConMonad Stanza
pull = pullPickle stanzaP

xmppFromHandle :: Handle
               -> Text
               -> Text
               -> Maybe Text
               -> XMPPConMonad a
               -> IO (a, XMPPConState)
xmppFromHandle handle hostname username res f = do
  liftIO $ hSetBuffering handle NoBuffering
  let raw = sourceHandle' handle
  let src = raw $= XP.parseBytes def
  let st = XMPPConState
             src
             (raw)
             (\xs -> CL.sourceList xs
                     $$ XR.renderBytes def =$ sinkHandle' handle)
             (BS.hPut handle)
             (Just handle)
             (SF Nothing [] [])
             False
             hostname
             username
             res
  runStateT f st

-- TODO: Once pullrequest has been merged, switch back to upstream
sourceHandle' :: MonadIO m => Handle -> Source m BS.ByteString
sourceHandle' h =
    src
  where
    src = PipeM pull close

    pull = do
        bs <- liftIO (BS.hGetSome h 4096)
        if BS.null bs
            then return $ Done Nothing ()
            else return $ HaveOutput src close bs

    close = return ()

sinkHandle' :: MonadIO m
           => Handle
           -> Sink BS.ByteString m ()
sinkHandle' h =
    NeedInput push close
  where
    push input = PipeM
        (liftIO (BS.hPut h input) >> return (NeedInput push close))
        (return ())
    close = return ()

xmppConnect :: HostName -> Text -> XMPPConMonad ()
xmppConnect host hostname = do
  uname <- gets sUsername
  con <- liftIO $ do
      con <- connectTo host (PortNumber 5222)
      hSetBuffering con NoBuffering
      return con
  let raw = sourceHandle' con
  let src = raw $= XP.parseBytes def
  let st = XMPPConState
             src
             (raw)
             (\xs -> CL.sourceList xs
                     $$ XR.renderBytes def =$ sinkHandle' con)
             (BS.hPut con)
             (Just con)
             (SF Nothing [] [])
             False
             hostname
             uname
             Nothing
  put st
  return ()

