{-# LANGUAGE OverloadedStrings #-}

module Network.XMPP.Monad where

import Control.Applicative((<$>))

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Resource
import Control.Monad.Trans.State

import Data.ByteString as BS
import Data.Text(Text)

import Data.Conduit
import Data.Conduit.Binary as CB
import Data.Conduit.Hexpat as CH
import Data.Conduit.List as CL
import Data.Conduit.Text as CT
import Data.Conduit.TLS

import Data.XML.Pickle
import Data.XML.Types
--import Text.XML.Stream.Parse as XP
import Text.XML.Stream.Render as XR
import Text.XML.Stream.Elements


import qualified Data.Text as Text

import Network.XMPP.Types
import Network.XMPP.Marshal
import Network.XMPP.Pickle

import System.IO

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
  lift $ source $$ snk

pullE :: XMPPMonad Element
pullE =  pulls elementFromEvents

pullPickle :: Show b => PU [Node] b -> XMPPMonad b
pullPickle p = unpickleElem p <$> pullE

pull :: XMPPMonad Stanza
pull = pullPickle stanzaP

xmppFromHandle
  :: Handle -> Text -> Text -> Maybe Text
     -> XMPPMonad a
     -> IO (a, XMPPState)
xmppFromHandle handle hostname username resource f = runResourceT $ do
  liftIO $ hSetBuffering handle NoBuffering
  raw <- bufferSource $ CB.sourceHandle handle
  src <- bufferSource $ raw $= CH.parseBS defaultParseOptions
  let st = XMPPState
             src
             raw
             (\xs -> CL.sourceList xs
                     $$ XR.renderBytes def =$ CB.sinkHandle handle)
             (BS.hPut handle)
             (Just handle)
             def
             False
             hostname
             username
             resource
  runStateT f st
