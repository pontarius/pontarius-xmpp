-- Copyright © 2010-2011 Jon Kristensen. See the LICENSE file in the Pontarius
-- XMPP distribution for more details.

{-# OPTIONS_HADDOCK hide #-}

{-# LANGUAGE OverloadedStrings #-}

-- TODO: Use -fno-cse? http://cvs.haskell.org/Hugs/pages/libraries/base/System-IO-Unsafe.html


-- TODO: Document this module
-- TODO: Make is possible to customize characters
-- TODO: Make it possible to customize length

module Network.XMPP.Utilities ( elementToString
                              , elementsToString, testElement ) where

import Prelude hiding (concat)
import Data.Word
import Data.XML.Types
import System.Crypto.Random
import System.Random
import qualified Data.ByteString as DB
import qualified Data.Map as DM
import qualified Data.Text as DT
import qualified Data.ByteString.Char8 as DBC

import Data.Enumerator (($$), Stream (Chunks), Enumerator, Iteratee, Step (Continue), continue, joinI,
                        run, run_, yield, returnI)
import Data.Enumerator.List (consume)
import Text.XML.Enumerator.Document (toEvents)
import Text.XML.Enumerator.Render (renderBytes)
import Data.Maybe (fromJust)
import Data.ByteString (concat, unpack)
import Data.List (tail)
import System.IO.Unsafe (unsafePerformIO)


{-# NOINLINE elementToString #-}


-- =============================================================================
--  XML Utilities
-- =============================================================================


-- TODO: Remove?

elementsToString :: [Element] -> String

elementsToString [] = ""
elementsToString (e:es) = (elementToString (Just e)) ++ (elementsToString es)


-- Converts the Element object to a document, converts it into Events, strips
-- the DocumentBegin event, generates a ByteString, and converts it into a
-- String.

elementToString :: Maybe Element -> String

elementToString Nothing = ""
elementToString (Just elem) = DBC.unpack $ concat $ unsafePerformIO $ do
    r <- run_ $ events $$ (joinI $ renderBytes $$ consume)
    return r
    where

        -- Enumerator that "produces" the events to convert to the document
        events :: Enumerator Event IO [DB.ByteString]
        events (Continue more) = more $ Chunks (tail $ toEvents $ dummyDoc elem)
        events step = returnI step

        dummyDoc :: Element -> Document
        dummyDoc e = Document (Prologue [] Nothing []) elem []


testElement :: Element
testElement = Element ("{http://example.com/ns/my-namespace}my-name" :: Name) [] []
