{-# OPTIONS_HADDOCK hide #-}
module Network.Xmpp.IM.Roster.Types where

import qualified Data.Map as Map
import           Data.Text (Text)
import           Network.Xmpp.Types

-- Note that `Remove' is not exported from IM.hs, as it will never be visible to
-- the user anyway.
data Subscription = None -- ^ the user does not have a subscription to the
                         -- contact's presence information, and the contact does
                         -- not have a subscription to the user's presence
                         -- information
                  | To  -- ^ the user has a subscription to the contact's
                        -- presence information, but the contact does not have a
                        -- subscription to the user's presence information
                  | From -- ^ the contact has a subscription to the user's
                         -- presence information, but the user does not have a
                         -- subscription to the contact's presence information
                  | Both -- ^ both the user and the contact have subscriptions
                         -- to each other's presence information
                  | Remove
                  deriving (Eq, Read, Show)

data Roster = Roster { ver :: Maybe Text
                     , items :: Map.Map Jid Item
                     } deriving Show

-- | Roster Items
data Item = Item { riApproved :: Bool
                 , riAsk :: Bool
                 , riJid :: Jid
                 , riName :: Maybe Text
                 , riSubscription :: Subscription
                 , riGroups :: [Text]
                 } deriving Show

data QueryItem = QueryItem { qiApproved :: Maybe Bool
                           , qiAsk :: Bool
                           , qiJid :: Jid
                           , qiName :: Maybe Text
                           , qiSubscription :: Maybe Subscription
                           , qiGroups :: [Text]
                           } deriving Show

data Query = Query { queryVer :: Maybe Text
                   , queryItems :: [QueryItem]
                   } deriving Show
