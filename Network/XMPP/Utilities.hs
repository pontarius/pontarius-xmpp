-- Copyright © 2010-2011 Jon Kristensen. See the LICENSE file in the Pontarius
-- XMPP distribution for more details.

-- This module currently converts XML elements to strings.

-- TODO: Use -fno-cse? http://cvs.haskell.org/Hugs/pages/libraries/base/System-IO-Unsafe.html
-- TODO: Remove elementsToString?

{-# OPTIONS_HADDOCK hide #-}

{-# LANGUAGE OverloadedStrings #-}

module Network.XMPP.Utilities ( elementToString
                              , elementsToString, testElement ) where

import Prelude hiding (concat)

import Data.ByteString (ByteString, concat)
import Data.ByteString.Char8 (unpack)
import Data.Enumerator (($$), Stream (Chunks), Enumerator, Step (Continue), joinI, run_, returnI)
import Data.Enumerator.List (consume)
import Data.XML.Types (Document (..), Element (..), Event (..), Name (..), Prologue (..))
import Text.XML.Enumerator.Render (renderBytes)
import Text.XML.Enumerator.Document (toEvents)

import System.IO.Unsafe (unsafePerformIO)


-- Converts the Element objects to a document, converts it into Events, strips
-- the DocumentBegin event, generates a ByteString, and converts it into a
-- String, aggregates the results and returns a string.

elementsToString :: [Element] -> String

elementsToString [] = ""
elementsToString (e:es) = (elementToString (Just e)) ++ (elementsToString es)


-- Converts the Element object to a document, converts it into Events, strips
-- the DocumentBegin event, generates a ByteString, and converts it into a
-- String.

{-# NOINLINE elementToString #-}

elementToString :: Maybe Element -> String

elementToString Nothing = ""
elementToString (Just elem) = unpack $ concat $ unsafePerformIO $ do
    r <- run_ $ events $$ (joinI $ renderBytes $$ consume)
    return r
    where

        -- Enumerator that "produces" the events to convert to the document
        events :: Enumerator Event IO [ByteString]
        events (Continue more) = more $ Chunks (tail $ toEvents $ dummyDoc elem)
        events step = returnI step

        dummyDoc :: Element -> Document
        dummyDoc e = Document (Prologue [] Nothing []) elem []
