{-# LANGUAGE OverloadedStrings #-}
module Test where

import qualified Data.ByteString as BS
import Data.Conduit
import Data.Default
import qualified Data.Conduit.List as CL
import qualified Text.XML.Stream.Parse as XP

xml =
   [ "<?xml version='1.0'?>"
   , "<stream:stream xmlns='jabber:client' "
   , "xmlns:stream='http://etherx.jabber.org/streams' id='1365401808' "
   , "from='examplehost.org' version='1.0' xml:lang='en'>"
   , "<stream:features>"
   , "<starttls xmlns='urn:ietf:params:xml:ns:xmpp-tls'/>"
   , error "Booh!"
   ] :: [BS.ByteString]

main :: IO ()
main = (runResourceT $ CL.sourceList xml $= XP.parseBytes def $$ CL.take 2 )
         >>= print