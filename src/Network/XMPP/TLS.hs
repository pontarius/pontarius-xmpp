{-# LANGUAGE OverloadedStrings  #-}

module Network.XMPP.TLS where

import Control.Monad(when)
import Control.Monad.Trans
import Control.Monad.Trans.State

import Network.XMPP.Monad
import Network.XMPP.Stream
import Network.TLSConduit as TLS

import Data.Conduit
import Data.Conduit.Text as CT
import Data.Conduit.List as CL
import qualified Data.List as L
import Data.XML.Types

import Text.XML.Stream.Elements
import Text.XML.Stream.Parse
import Text.XML.Stream.Render as XR


starttlsE =
  Element (Name "starttls" (Just "urn:ietf:params:xml:ns:xmpp-tls") Nothing ) [] []

exampleParams = TLS.defaultParams {TLS.pCiphers = TLS.ciphersuite_strong}

xmppStartTLS params = do
  features <- gets sFeatures
  when (stls features) $ do
      push starttlsE
      Element "{urn:ietf:params:xml:ns:xmpp-tls}proceed" [] [] <- pull
      Just handle <- gets conHandle
      (src', snk) <- lift $ TLS.tlsinit params handle
      src <- lift . bufferSource $ src' $= CT.decode CT.utf8 $= parseText def
      modify (\x -> x
                     { conSrc = src
                     , conSink = XR.renderBytes def =$ snk
                     })
      xmppStartStream
      modify (\s -> s{haveTLS = True})
  gets haveTLS

