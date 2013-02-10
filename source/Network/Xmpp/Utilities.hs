{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

{-# OPTIONS_HADDOCK hide #-}

module Network.Xmpp.Utilities (idGenerator, presTo, message, answerMessage) where

import Network.Xmpp.Types

import Control.Monad.STM
import Control.Concurrent.STM.TVar
import Prelude

import Data.XML.Types

import qualified Data.Attoparsec.Text as AP
import qualified Data.Text as Text


-- | Creates a new @IdGenerator@. Internally, it will maintain an infinite list
-- of IDs ('[\'a\', \'b\', \'c\'...]'). The argument is a prefix to prepend the
-- IDs with. Calling the function will extract an ID and update the generator's
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
    -- provided prefix. Adds the prefix to all combinations of IDs (ids').
    ids :: Text.Text -> [Text.Text]
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

-- Constructs a "Version" based on the major and minor version numbers.
versionFromNumbers :: Integer -> Integer -> Version
versionFromNumbers major minor = Version major minor

-- | Add a recipient to a presence notification.
presTo :: Presence -> Jid -> Presence
presTo pres to = pres{presenceTo = Just to}

-- | An empty message.
message :: Message
message = Message { messageID      = Nothing
                  , messageFrom    = Nothing
                  , messageTo      = Nothing
                  , messageLangTag = Nothing
                  , messageType    = Normal
                  , messagePayload = []
                  }

-- Produce an answer message with the given payload, switching the "from" and
-- "to" attributes in the original message.
answerMessage :: Message -> [Element] -> Maybe Message
answerMessage Message{messageFrom = Just frm, ..} payload =
    Just Message{ messageFrom    = messageTo
                , messageID      = Nothing
                , messageTo      = Just frm
                , messagePayload = payload
                , ..
                }
answerMessage _ _ = Nothing
