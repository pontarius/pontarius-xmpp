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
import Data.Conduit.List as CL
import Data.Conduit.Text as CT
import Data.Conduit.TLS

import Data.XML.Pickle
import Data.XML.Types
import Text.XML.Stream.Parse as XP
import Text.XML.Stream.Render as XR
import Text.XML.Stream.Elements

import qualified Data.Text as Text

import Network.XMPP.Types
import Network.XMPP.Marshal
import Network.XMPP.Pickle

import System.IO

-- parseOpts :: ParseOptions tag text
-- parseOpts = ParseOptions (Just UTF8) Nothing

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
pullE = do
  pulls elementFromEvents

pullPickle :: PU [Node] b -> XMPPMonad b
pullPickle p = unpickleElem p <$> pullE

pull :: XMPPMonad Stanza
pull = pullPickle stanzaP

-- pull :: XMPPMonad Stanza
-- pull = elementToStanza <$> pullE

xmppFromHandle
  :: Handle -> Text -> Text -> Maybe Text
     -> XMPPMonad a
     -> IO (a, XMPPState)
xmppFromHandle handle hostname username resource f = runResourceT $ do
  liftIO $ hSetBuffering handle NoBuffering
  let raw = CB.sourceHandle handle --  $= conduitStdout
  liftIO $ BS.hPut handle "<stream:stream xmlns=\"jabber:client\" xmlns:stream=\"http://etherx.jabber.org/streams\" version=\"1.0\" to=\"species64739.dyndns.org\">"
  src <- bufferSource $ raw $= CT.decode CT.utf8 $= XP.parseText def
  src $= CL.map (Text.pack . show) $= CT.encode CT.utf8  $$ sinkHandle stdout
  error "done"
  let st = XMPPState
             src
             undefined -- raw
             (\xs -> CL.sourceList xs
                     $$ XR.renderBytes def =$ conduitStdout =$ CB.sinkHandle handle)
             (BS.hPut handle)
             (Just handle)
             def
             False
             hostname
             username
             resource
  runStateT f st

xml =
   [ "<?xml version='1.0'?>"
   , "<stream:stream xmlns='JABBER15:client' "
   , "xmlns:stream='http://etherx.jabber.org/streams' id='1365401808' "
   , "from='species64739.dyndns.org' version='1.0' xml:lang='en'>"
   , "<stream:features>"
   , "<starttls xmlns='urn:ietf:params:xml:ns:xmpp-tls'/>"
   , "<mechanisms xmlns='urn:ietf:params:xml:ns:xmpp-sasl'>"
   , "<mechanism>PLAIN"
   , "</mechanism>"
   , "<mechanism>DIGEST-MD5"
   , "</mechanism>"
   , "<mechanism>SCRAM-SHA-1"
   , "</mechanism>"
   , "</mechanisms>"
   , "<c xmlns='http://jabber.org/protocol/caps' hash='sha-1' node='http://www.process-one.net/en/ejabberd/' ver='yy7di5kE0syuCXOQTXNBTclpNTo='/>"
   , "<register xmlns='http://jabber.org/features/iq-register'/>"
   , "</stream:features>"
   , error "Booh!"
   ] :: [ByteString]

xml2 = BS.concat ["<?xml version='1.0'?><stream:stream xmlns='jabber:client' xmlns:stream='http://etherx.jabber.org/streams' id='2181744549' from='species64739.dyndns.org' version='1.0' xml:lang='en'>"
  ,"<stream:features><starttls xmlns='urn:ietf:params:xml:ns:xmpp-tls'/><mechanisms xmlns='urn:ietf:params:xml:ns:xmpp-sasl'><mechanism>PLAIN</mechanism><mechanism>DIGEST-MD5</mechanism><mechanism>SCRAM-SHA-1</mechanism></mechanisms><c xmlns='http://jabber.org/protocol/caps' hash='sha-1' node='http://www.process-one.net/en/ejabberd/' ver='yy7di5kE0syuCXOQTXNBTclpNTo='/><register xmlns='http://jabber.org/features/iq-register'/></stream:features>"]

fooS sr = sr $= CT.decode CT.utf8 $= XP.parseText def
blarg = forever $ do
      p <- CL.peek
      case p of
        Nothing -> error "end"
        Just p' -> liftIO $ print p
      CL.drop 1


test :: Source (ResourceT IO) ByteString -> ResourceT IO ()
test sr = fooS sr $$ blarg