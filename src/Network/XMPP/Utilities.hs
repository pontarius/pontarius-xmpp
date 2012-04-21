-- Copyright © 2010-2012 Jon Kristensen. See the LICENSE file in the Pontarius
-- distribution for more details.

-- TODO: More efficient to use Text instead of Strings for ID generation?

{-# OPTIONS_HADDOCK hide #-}

{-# LANGUAGE OverloadedStrings #-}

module Network.XMPP.Utilities (idGenerator) where

import Network.XMPP.Types

import Control.Monad.STM
import Control.Concurrent.STM.TVar
import Prelude

import qualified Data.Attoparsec.Text as AP
import qualified Data.Text as Text


-- |
-- Creates a new @IdGenerator@. Internally, it will maintain an infinite list of
-- IDs ('[\'a\', \'b\', \'c\'...]'). The argument is a prefix to prepend the IDs
-- with. Calling the function will extract an ID and update the generator's
-- internal state so that the same ID will not be generated again.

idGenerator :: Text.Text -> IO IdGenerator

idGenerator prefix = atomically $ do
    tvar <- newTVar $ ids prefix
    return $ IdGenerator $ next tvar

  where

    -- Transactionally extract the next ID from the infinite list of IDs.
    
    next :: TVar [Text.Text] -> IO Text.Text
    next tvar = atomically $ do
        list <- readTVar tvar
        case list of
          [] -> error "empty list in Utilities.hs"
          (x:xs) -> do
            writeTVar tvar xs
            return x

    -- Generates an infinite and predictable list of IDs, all beginning with the
    -- provided prefix.
    
    ids :: Text.Text -> [Text.Text]

    -- Adds the prefix to all combinations of IDs (ids').
    ids p = map (\ id -> Text.append p id) ids'
      where
        
        -- Generate all combinations of IDs, with increasing length.
        ids' :: [Text.Text]
        ids' = map Text.pack $ concatMap ids'' [1..]

        -- Generates all combinations of IDs with the given length.
        ids'' :: Integer -> [String]
        ids'' 0 = [""]
        ids'' l = [x:xs | x <- repertoire, xs <- ids'' (l - 1)]

        -- Characters allowed in IDs.
        repertoire :: String
        repertoire = ['a'..'z']


-- Converts a "<major>.<minor>" numeric version number to a @Version@ object.
versionFromString :: Text.Text -> Maybe Version
versionFromString s = case AP.parseOnly versionParser s of
                        Right version -> Just version
                        Left _ -> Nothing


-- Constructs a "Version" based on the major and minor version numbers.
versionFromNumbers :: Integer -> Integer -> Version
versionFromNumbers major minor = Version major minor


-- Read numbers, a dot, more numbers, and end-of-file.
versionParser :: AP.Parser Version
versionParser = do
    major <- AP.many1 AP.digit
    AP.skip (\ c -> c == '.')
    minor <- AP.many1 AP.digit
    AP.endOfInput
    return $ Version (read major) (read minor)