{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

{-# OPTIONS_HADDOCK hide #-}

module Network.Xmpp.Utilities (presTo, message, answerMessage, openElementToEvents, renderOpenElement, renderElement) where

import Network.Xmpp.Types

import Control.Monad.STM
import Control.Concurrent.STM.TVar
import Prelude

import Data.XML.Types

import qualified Data.Attoparsec.Text as AP
import qualified Data.Text as Text

import qualified Data.ByteString as BS
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import           System.IO.Unsafe(unsafePerformIO)
import           Data.Conduit.List as CL
-- import           Data.Typeable
import           Control.Applicative ((<$>))
import           Control.Exception
import           Control.Monad.Trans.Class

import           Data.Conduit as C
import           Data.XML.Types

import qualified Text.XML.Stream.Render as TXSR
import           Text.XML.Unresolved as TXU


-- TODO: Not used, and should probably be removed.
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
    ids p = Prelude.map (\ id -> Text.append p id) ids'
      where
        -- Generate all combinations of IDs, with increasing length.
        ids' :: [Text.Text]
        ids' = Prelude.map Text.pack $ Prelude.concatMap ids'' [1..]
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

-- | Produce an answer message with the given payload, switching the "from" and
-- "to" attributes in the original message. Produces a 'Nothing' value of the
-- provided message message has no from attribute.
answerMessage :: Message -> [Element] -> Maybe Message
answerMessage Message{messageFrom = Just frm, ..} payload =
    Just Message{ messageFrom    = messageTo
                , messageID      = Nothing
                , messageTo      = Just frm
                , messagePayload = payload
                , ..
                }
answerMessage _ _ = Nothing

openElementToEvents :: Element -> [Event]
openElementToEvents (Element name as ns) = EventBeginElement name as : goN ns []
  where
    goE (Element name' as' ns') =
          (EventBeginElement name' as' :)
        . goN ns'
        . (EventEndElement name' :)
    goN [] = id
    goN [x] = goN' x
    goN (x:xs) = goN' x . goN xs
    goN' (NodeElement e) = goE e
    goN' (NodeInstruction i) = (EventInstruction i :)
    goN' (NodeContent c) = (EventContent c :)
    goN' (NodeComment t) = (EventComment t :)

renderOpenElement :: Element -> BS.ByteString
renderOpenElement e = Text.encodeUtf8 . Text.concat . unsafePerformIO
    $ CL.sourceList (openElementToEvents e) $$ TXSR.renderText def =$ CL.consume

renderElement :: Element -> BS.ByteString
renderElement e = Text.encodeUtf8 . Text.concat . unsafePerformIO
    $ CL.sourceList (elementToEvents e) $$ TXSR.renderText def =$ CL.consume
  where
    elementToEvents :: Element -> [Event]
    elementToEvents e@(Element name _ _) = openElementToEvents e ++ [EventEndElement name]
