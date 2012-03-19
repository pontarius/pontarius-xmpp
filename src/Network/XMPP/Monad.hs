module Network.XMPP.Monad where

import Control.Applicative((<$>))

import Control.Monad.Trans
import Control.Monad.Trans.State

import Data.Conduit
import Data.Conduit.Text as CT
import Data.Conduit.Binary as CB
import Data.Conduit.List as CL
import Data.XML.Types

import Data.Default
import Data.Text

import Network.XMPP.Types
import Network.XMPP.Marshal

import System.IO

import Text.XML.Stream.Elements
import Text.XML.Stream.Render as XR
import Text.XML.Stream.Parse

type XMPPMonad a = StateT XMPPState (ResourceT IO) a

data XMPPState = XMPPState
                   { sConSrc    :: BufferedSource IO Event
                   , sConSink   :: Sink Event IO ()
                   , sConHandle :: Maybe Handle
                   , sFeatures :: ServerFeatures
                   , sHaveTLS   :: Bool
                   , sHostname :: Text
                   , sUsername  :: Text
                   , sResource  :: Maybe Text
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


pushE :: Element -> XMPPMonad ()
pushE x = do
  sink <- gets sConSink
  lift $ CL.sourceList (elementToEvents x) $$ sink

push :: Stanza -> XMPPMonad ()
push = pushE . stanzaToElement

pushOpen :: Element -> XMPPMonad ()
pushOpen x = do
  sink <- gets sConSink
  lift $ CL.sourceList (elementToEvents' x) $$ sink


pulls :: Sink Event IO a -> XMPPMonad a
pulls snk = do
  source <- gets sConSrc
  lift $ source $$ snk

pullE :: XMPPMonad Element
pullE = do
  source <- gets sConSrc
  pulls elementFromEvents

pull :: XMPPMonad Stanza
pull = elementToStanza <$> pullE

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
