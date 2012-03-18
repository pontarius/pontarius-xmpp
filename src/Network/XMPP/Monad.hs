module Network.XMPP.Monad where

import Control.Monad.Trans
import Control.Monad.Trans.State

import Data.Conduit
import Data.Conduit.List as CL
import Data.XML.Types

import Data.Default
import Data.Text

import System.IO

import Text.XML.Stream.Elements

type XMPPMonad a = StateT XMPPState (ResourceT IO) a

data XMPPState = XMPPState
                   { conSrc    :: BufferedSource IO Event
                   , conSink   :: Sink Event IO ()
                   , conHandle :: Maybe Handle
                   , sFeatures :: ServerFeatures
                   , haveTLS   :: Bool
                   , sHostname  :: Text
                   , jid       :: Text
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
