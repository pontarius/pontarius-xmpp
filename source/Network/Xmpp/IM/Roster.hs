{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.Xmpp.IM.Roster
where

import Control.Concurrent.STM
import Control.Monad
import Data.Text (Text)
import Data.XML.Pickle
import Data.XML.Types
import Network.Xmpp
import Network.Xmpp.Marshal
import System.Log.Logger
import qualified Data.Map.Strict as Map

import Network.Xmpp.Types

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
                     , items :: Map.Map Jid Item }


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


withRoster :: Maybe Roster
           -> SessionConfiguration
           -> (SessionConfiguration -> IO (Either XmppFailure Session))
           -> IO (Either XmppFailure (TVar Roster, Session))
withRoster oldRoster conf startSession = do
    rosterRef <- newTVarIO $ Roster Nothing Map.empty
    mbSess <- startSession conf{extraStanzaHandlers = handleRoster rosterRef :
                                                   extraStanzaHandlers conf}
    case mbSess of
        Left e -> return $ Left e
        Right sess -> do
            mbRoster <- getRoster oldRoster sess
            case mbRoster of
                Nothing -> errorM "Pontarius.Xmpp" "Server did not return a roster"
                Just roster -> atomically $ writeTVar rosterRef roster
            return $ Right (rosterRef, sess)

handleRoster :: TVar Roster -> TChan Stanza -> Stanza -> IO Bool
handleRoster rosterRef outC sta = do
    case sta of
        IQRequestS (iqr@IQRequest{iqRequestPayload =
                                       iqb@Element{elementName = en}})
            | nameNamespace en == Just "jabber:iq:roster" -> do
                case iqRequestFrom iqr of
                    Just _from -> return True -- Don't handle roster pushes from
                                              -- unauthorized sources
                    Nothing -> case unpickleElem xpQuery iqb of
                        Right Query{ queryVer = v
                                   , queryItems = [update]
                                   } -> do
                            handleUpdate v update
                            atomically . writeTChan outC $ result iqr
                            return False
                        _ -> do
                            errorM "Pontarius.Xmpp" "Invalid roster query"
                            atomically . writeTChan outC $ badRequest iqr
                            return False
        _ -> return True
  where
    handleUpdate v' update = atomically $ modifyTVar rosterRef $ \(Roster v is) ->
        Roster (v' `mplus` v) $ case qiSubscription update of
            Just Remove -> Map.delete (qiJid update) is
            _ -> Map.insert (qiJid update) (toItem update) is

    badRequest (IQRequest iqid from _to lang _tp bd) =
        IQErrorS $ IQError iqid Nothing from lang errBR (Just bd)
    errBR = StanzaError Cancel BadRequest Nothing Nothing
    result (IQRequest iqid from _to lang _tp _bd) =
        IQResultS $ IQResult iqid Nothing from lang Nothing

getRoster :: Maybe Roster -> Session -> IO (Maybe Roster)
getRoster oldRoster sess = do
    res <- sendIQ' Nothing Get Nothing
        (pickleElem xpQuery (Query (ver =<< oldRoster) []))
        sess
    case res of
        IQResponseResult (IQResult{iqResultPayload = Just ros})
            -> case unpickleElem xpQuery ros of
            Left _e -> do
                errorM "Pontarius.Xmpp.Roster" "getRoster: invalid query element"
                return Nothing
            Right roster -> return . Just $ toRoster roster
        IQResponseResult (IQResult{iqResultPayload = Nothing}) -> do
            return $ oldRoster
                -- sever indicated that no roster updates are necessary
        IQResponseTimeout -> do
            errorM "Pontarius.Xmpp.Roster" "getRoster: request timed out"
            return Nothing
        IQResponseError e -> do
            errorM "Pontarius.Xmpp.Roster" $ "getRoster: server returned error"
                   ++ show e
            return Nothing
  where
    toRoster (Query v is) = Roster v (Map.fromList
                                             $ map (\i -> (qiJid i, toItem i))
                                               is)

toItem :: QueryItem -> Item
toItem qi = Item { approved = maybe False id (qiApproved qi)
                 , ask = qiAsk qi
                 , jid = qiJid qi
                 , name = qiName qi
                 , subscription = maybe None id (qiSubscription qi)
                 , groups = qiGroups qi
                 }

xpItems :: PU [Node] [QueryItem]
xpItems = xpWrap (map (\((app_, ask_, jid_, name_, sub_), groups_) ->
                        QueryItem app_ ask_ jid_ name_ sub_ groups_))
                 (map (\(QueryItem app_ ask_ jid_ name_ sub_ groups_) ->
                        ((app_, ask_, jid_, name_, sub_), groups_))) $
          xpElems "{jabber:iq:roster}item"
          (xp5Tuple
            (xpAttribute' "approved" xpBool)
            (xpWrap (maybe False (const True))
                    (\p -> if p then Just () else Nothing) $
                     xpOption $ xpAttribute_ "ask" "subscribe")
            (xpAttribute  "jid" xpPrim)
            (xpAttribute' "name" xpText)
            (xpAttribute' "subscription" xpPrim)
          )
          (xpFindMatches $ xpElemText "{jabber:iq:roster}group")

xpQuery :: PU [Node] Query
xpQuery = xpWrap (\(ver_, items_) -> Query ver_ items_ )
                 (\(Query ver_ items_) -> (ver_, items_)) $
          xpElem "{jabber:iq:roster}query"
            (xpAttribute' "ver" xpText)
            xpItems
