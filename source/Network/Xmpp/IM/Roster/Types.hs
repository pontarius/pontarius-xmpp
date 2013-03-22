module Network.Xmpp.IM.Roster.Types where

import qualified Data.Map as Map
import           Data.Text (Text)
import           Network.Xmpp.Types

data Subscription = None | To | From | Both | Remove deriving Eq

instance Show Subscription where
    show None = "none"
    show To   = "to"
    show From = "from"
    show Both = "both"
    show Remove = "remove"

instance Read Subscription where
    readsPrec _ "none" = [(None ,"")]
    readsPrec _ "to"   = [(To   ,"")]
    readsPrec _ "from" = [(From ,"")]
    readsPrec _ "both" = [(Both ,"")]
    readsPrec _ "remove" = [(Remove ,"")]
    readsPrec _ _ = []

data Roster = Roster { ver :: Maybe Text
                     , items :: Map.Map Jid Item } deriving Show



data Item = Item { approved :: Bool
                 , ask :: Bool
                 , jid :: Jid
                 , name :: Maybe Text
                 , subscription :: Subscription
                 , groups :: [Text]
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
