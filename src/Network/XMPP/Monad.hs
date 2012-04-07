{-# LANGUAGE OverloadedStrings #-}

module Network.XMPP.Monad where

import Control.Applicative((<$>))
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Resource
import Control.Monad.Trans.State

import Data.ByteString as BS
import Data.Conduit
import Data.Conduit.Binary as CB
import Data.Conduit.List as CL
import Data.Text(Text)
import Data.XML.Pickle
import Data.XML.Types

import Network.XMPP.Types
import Network.XMPP.Marshal
import Network.XMPP.Pickle

import System.IO

import Text.XML.Stream.Elements
import Text.XML.Stream.Parse as XP
import Text.XML.Stream.Render as XR


pushN :: Element -> XMPPMonad ()
pushN x = do
  sink <- gets sConPush
  lift . sink $ elementToEvents x

push :: Stanza -> XMPPMonad ()
push = pushN . pickleElem stanzaP

pushOpen :: Element -> XMPPMonad ()
pushOpen e = do
  sink <- gets sConPush
  lift . sink $ openElementToEvents e
  return ()

pulls :: Sink Event (ResourceT IO) a -> XMPPMonad a
pulls snk = do
  source <- gets sConSrc
  (src', r) <- lift $ source $$+ snk
  modify $ (\s -> s {sConSrc = src'})
  return r

pullE :: XMPPMonad Element
pullE = pulls elementFromEvents

pullPickle :: Show b => PU [Node] b -> XMPPMonad b
pullPickle p = unpickleElem p <$> pullE

pull :: XMPPMonad Stanza
pull = pullPickle stanzaP

xmppFromHandle
  :: Handle -> Text -> Text -> Maybe Text
     -> XMPPMonad a
     -> IO (a, XMPPState)
xmppFromHandle handle hostname username res f = runResourceT $ do
  liftIO $ hSetBuffering handle NoBuffering
  let raw = CB.sourceHandle handle
  let src = raw $= XP.parseBytes def
  let st = XMPPState
             src
             (raw)
             (\xs -> CL.sourceList xs
                     $$ XR.renderBytes def =$ CB.sinkHandle handle)
             (BS.hPut handle)
             (Just handle)
             def
             False
             hostname
             username
             res
  runStateT f st

