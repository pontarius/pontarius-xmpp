{-# OPTIONS_HADDOCK hide #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.Xmpp.IM.Roster where

import           Control.Concurrent.STM
import           Control.Monad
import qualified Data.Map.Strict as Map
import           Data.Maybe (isJust)
import           Data.XML.Pickle
import           Data.XML.Types
import           System.Log.Logger

import           Network.Xmpp.IM.Roster.Types
import           Network.Xmpp.Marshal
import           Network.Xmpp.Concurrent.Types
import           Network.Xmpp.Types
import           Network.Xmpp.Concurrent.IQ

getRoster :: Session -> IO Roster
getRoster session = atomically $ readTVar (rosterRef session)

initRoster :: Session -> IO ()
initRoster session = do
    oldRoster <- getRoster session
    mbRoster <- retrieveRoster (if isJust (ver oldRoster) then Just oldRoster
                                                          else Nothing ) session
    case mbRoster of
        Nothing -> errorM "Pontarius.Xmpp"
                          "Server did not return a roster"
        Just roster -> atomically $ writeTVar (rosterRef session) roster

handleRoster :: TVar Roster -> TChan Stanza -> Stanza -> IO Bool
handleRoster ref outC sta = do
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
    handleUpdate v' update = atomically $ modifyTVar ref $ \(Roster v is) ->
        Roster (v' `mplus` v) $ case qiSubscription update of
            Just Remove -> Map.delete (qiJid update) is
            _ -> Map.insert (qiJid update) (toItem update) is

    badRequest (IQRequest iqid from _to lang _tp bd) =
        IQErrorS $ IQError iqid Nothing from lang errBR (Just bd)
    errBR = StanzaError Cancel BadRequest Nothing Nothing
    result (IQRequest iqid from _to lang _tp _bd) =
        IQResultS $ IQResult iqid Nothing from lang Nothing

retrieveRoster :: Maybe Roster -> Session -> IO (Maybe Roster)
retrieveRoster oldRoster sess = do
    res <- sendIQ' Nothing Get Nothing
        (pickleElem xpQuery (Query (ver =<< oldRoster) []))
        sess
    case res of
        IQResponseResult (IQResult{iqResultPayload = Just ros})
            -> case unpickleElem xpQuery ros of
            Left _e -> do
                errorM "Pontarius.Xmpp.Roster" "getRoster: invalid query element"
                return Nothing
            Right ros' -> return . Just $ toRoster ros'
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
