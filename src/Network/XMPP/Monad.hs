module Network.XMPP.Monad where

import Control.Monad.Trans
import Control.Monad.Trans.State

import Data.Conduit
import Data.Conduit.Text as CT
import Data.Conduit.Binary as CB
import Data.Conduit.List as CL
import Data.XML.Types

import Data.Default
import Data.Text

import System.IO

import Text.XML.Stream.Elements
import Text.XML.Stream.Render as XR
import Text.XML.Stream.Parse

type XMPPMonad a = StateT XMPPState (ResourceT IO) a

data XMPPState = XMPPState
                   { conSrc    :: BufferedSource IO Event
                   , conSink   :: Sink Event IO ()
                   , conHandle :: Maybe Handle
                   , sFeatures :: ServerFeatures
                   , haveTLS   :: Bool
                   , sHostname :: Text
                   , username  :: Text
                   , resource  :: Text
                   }

data ServerFeatures = SF
  { stls  :: Bool
  , stlsRequired :: Bool
  , saslMechanisms :: [Text]
  , other :: [Element]
  } deriving Show

instance Default ServerFeatures where
  def = SF
          { stls  = False
          , stlsRequired = False
          , saslMechanisms = []
          , other = []
          }

push :: Element -> XMPPMonad ()
push x = do
  sink <- gets conSink
  lift $ CL.sourceList (elementToEvents x) $$ sink

pushOpen :: Element -> XMPPMonad ()
pushOpen x = do
  sink <- gets conSink
  lift $ CL.sourceList (elementToEvents' x) $$ sink


pulls :: Sink Event IO a -> XMPPMonad a
pulls snk = do
  source <- gets conSrc
  lift $ source $$ snk

pull :: XMPPMonad Element
pull = do
  source <- gets conSrc
  pulls elementFromEvents

xmppFromHandle handle hostname username resource f = runResourceT $ do
  liftIO $ hSetBuffering handle NoBuffering
  src <- bufferSource $ CB.sourceHandle handle $= CT.decode CT.utf8 $= parseText def
  let st = XMPPState
             src
             (XR.renderBytes def =$ CB.sinkHandle handle)
             (Just handle)
             def
             False
             hostname
             username
             resource
  runStateT f st
