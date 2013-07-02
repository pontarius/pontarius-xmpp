module Network.Xmpp.IM.Roster.Types where

import qualified Data.Map as Map
import           Data.Text (Text)
import           Network.Xmpp.Types

data Subscription = None | To | From | Both | Remove deriving (Eq, Read, Show)

data Roster = Roster { ver :: Maybe Text
                     , items :: Map.Map Jid Item } deriving Show


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
