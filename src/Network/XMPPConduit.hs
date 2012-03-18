{-# LANGUAGE NoMonomorphismRestriction, OverloadedStrings  #-}
module Network.XMPPConduit where

import Control.Exception
import Control.Monad
import Control.Monad.ST (runST)
import Control.Monad.Trans
import Control.Monad.Trans.Class
import Control.Monad.Trans.State
import Control.Applicative


import Data.Conduit as C
import Data.Conduit.Binary as CB
import Data.Conduit.Text as CT
import Data.Default
import Data.List as L
import Data.Text as T
import Data.XML.Types

import GHC.IO.Handle

import Network
import qualified Network.TLSConduit as TLS

import System.IO
import System.Random

import Text.XML.Stream.Elements
import Text.XML.Stream.Render as XR
import Text.XML.Stream.Parse

import qualified Data.Conduit.List as CL


xmppSASL = do
  return ()

xmppFromHandle handle hostname jid = do
  liftIO $ hSetBuffering handle NoBuffering
  src <- bufferSource $ CB.sourceHandle handle $= CT.decode CT.utf8 $= parseText def
  let st = XMPPState
             src
             (XR.renderBytes def =$ CB.sinkHandle handle)
             (Just handle)
             def
             False
             hostname
             jid
  flip runStateT st $ do
      xmppStartStream
      xmppStartTLS
      xmppSASL

main = do
  con <- connectTo "localhost" (PortNumber 5222)
  hSetBuffering con NoBuffering
  fs <- runResourceT $ xmppFromHandle con "species_64739.dyndns.org" "uart14"
  putStrLn ""
  hGetContents con >>= putStrLn

