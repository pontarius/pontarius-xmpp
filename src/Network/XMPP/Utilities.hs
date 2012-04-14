-- Copyright © 2010-2012 Jon Kristensen. See the LICENSE file in the
-- Pontarius distribution for more details.

-- This module currently converts XML elements to strings.

-- TODO: Use -fno-cse? http://cvs.haskell.org/Hugs/pages/libraries/base/System-IO-Unsafe.html
-- TODO: Remove elementsToString?

{-# OPTIONS_HADDOCK hide #-}

{-# LANGUAGE OverloadedStrings #-}

module Network.XMPP.Utilities ( idGenerator
                              , nextId
  -- elementToString
                              -- , elementsToString ) where
  ) where

import Network.XMPP.Types

import Prelude hiding (concat)

import Data.ByteString (ByteString, concat)
import Data.ByteString.Char8 (unpack)

import Data.Enumerator (($$), Stream (Chunks), Enumerator, Step (Continue), joinI, run_, returnI)
import Data.Enumerator.List (consume)

import Data.XML.Types (Document (..), Element (..), Event (..), Name (..), Prologue (..))

import Data.IORef (atomicModifyIORef, newIORef)


-- import Text.XML.Enumerator.Render (renderBytes)
-- import Text.XML.Enumerator.Document (toEvents)

import System.IO.Unsafe (unsafePerformIO)


-- |
-- Creates a new stanza "IdGenerator". Internally, it will maintain an infinite
-- list of stanza IDs ('[\'a\', \'b\', \'c\'...]').

idGenerator :: String -> IO IdGenerator

idGenerator p = newIORef (ids p) >>= \ ioRef -> return $ IdGenerator ioRef

  where

    -- Generates an infinite and predictable list of IDs, all
    -- beginning with the provided prefix.
    
    ids :: String -> [String]

    -- Adds the prefix to all combinations of IDs (ids').
    ids p = map (\ id -> p ++ id) ids'
      where
        
        -- Generate all combinations of IDs, with increasing length.
        ids' :: [String]
        ids' = concatMap ids'' [1..]

        -- Generates all combinations of IDs with the given length.
        ids'' :: Integer -> [String]
        ids'' 0 = [""]
        ids'' l = [x:xs | x <- repertoire, xs <- ids'' (l - 1)]

        -- Characters allowed in IDs.
        repertoire :: String
        repertoire = ['a'..'z']



-- |
-- Extracts an ID from the "IDGenerator", and updates the generators internal
-- state so that the same ID will not be generated again.

nextId :: IdGenerator -> IO String

nextId g = let IdGenerator ioRef = g
           in atomicModifyIORef ioRef (\ (i:is) -> (is, i))



-- Converts the Element objects to a document, converts it into Events, strips
-- the DocumentBegin event, generates a ByteString, and converts it into a
-- String, aggregates the results and returns a string.

-- elementsToString :: [Element] -> String

-- elementsToString [] = ""
-- elementsToString (e:es) = (elementToString (Just e)) ++ (elementsToString es)


-- Converts the Element object to a document, converts it into Events, strips
-- the DocumentBegin event, generates a ByteString, and converts it into a
-- String.

-- {-# NOINLINE elementToString #-}

-- elementToString :: Maybe Element -> String

-- elementToString Nothing = ""
-- elementToString (Just elem) = unpack $ concat $ unsafePerformIO $ do
--     r <- run_ $ events $$ (joinI $ renderBytes $$ consume)
--     return r
--     where

        -- Enumerator that "produces" the events to convert to the document
--         events :: Enumerator Event IO [ByteString]
--         events (Continue more) = more $ Chunks (tail $ toEvents $ dummyDoc elem)
--         events step = returnI step

--         dummyDoc :: Element -> Document
--         dummyDoc e = Document (Prologue [] Nothing []) elem []
