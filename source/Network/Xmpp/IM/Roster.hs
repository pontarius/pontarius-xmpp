{-# OPTIONS_HADDOCK hide #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.Xmpp.IM.Roster where

import           Control.Applicative ((<$>))
import           Control.Concurrent.STM
import           Control.Monad
import           Data.List (nub)
#if MIN_VERSION_containers(0, 5, 0)
import qualified Data.Map.Strict as Map
#else
import qualified Data.Map as Map
#endif
import           Data.Maybe (isJust, fromMaybe)
import           Data.Text (Text)
import           Data.XML.Pickle
import           Data.XML.Types
import           System.Log.Logger

import           Network.Xmpp.Concurrent.Basic
import           Network.Xmpp.Concurrent.IQ
import           Network.Xmpp.Concurrent.Types
import           Network.Xmpp.IM.Roster.Types
import           Network.Xmpp.Marshal
import           Network.Xmpp.Types

-- | Push a roster item to the server. The values for approved and ask are
-- ignored and all values for subsciption except "remove" are ignored
rosterPush :: Item -> Session -> IO (Maybe IQResponse)
rosterPush item session = do
    let el = pickleElem xpQuery (Query Nothing [fromItem item])
    sendIQ'  Nothing Set Nothing el session

-- | Add or update an item to the roster.
--
-- To update the item just send the complete set of new data
rosterAdd :: Jid -- ^ JID of the item
          -> Maybe Text -- ^ Name alias
          -> [Text] -- ^ Groups (duplicates will be removed)
          -> Session
          -> IO (Maybe IQResponse)
rosterAdd j n gs session = do
    let el = pickleElem xpQuery (Query Nothing
                                 [QueryItem { qiApproved = Nothing
                                            , qiAsk = False
                                            , qiJid = j
                                            , qiName = n
                                            , qiSubscription = Nothing
                                            , qiGroups = nub gs
                                            }])
    sendIQ'  Nothing Set Nothing el session

-- | Remove an item from the roster. Return True when the item is sucessfully
-- removed or if it wasn't in the roster to begin with.
rosterRemove :: Jid -> Session -> IO Bool
rosterRemove j sess = do
    roster <- getRoster sess
    case Map.lookup j (items roster) of
        Nothing -> return True -- jid is not on the Roster
        Just _ -> do
            res <- rosterPush (Item False False j Nothing Remove []) sess
            case res of
                Just (IQResponseResult IQResult{}) -> return True
                _ -> return False

-- | Retrieve the current Roster state
getRoster :: Session -> IO Roster
getRoster session = atomically $ readTVar (rosterRef session)

-- | Get the initial roster / refresh the roster. You don't need to call this on your own
initRoster :: Session -> IO ()
initRoster session = do
    oldRoster <- getRoster session
    mbRoster <- retrieveRoster (if isJust (ver oldRoster) then Just oldRoster
                                                          else Nothing ) session
    case mbRoster of
        Nothing -> errorM "Pontarius.Xmpp"
                          "Server did not return a roster"
        Just roster -> atomically $ writeTVar (rosterRef session) roster

handleRoster :: TVar Roster -> WriteSemaphore -> Stanza -> IO [Stanza]
handleRoster ref sem sta = case sta of
    IQRequestS (iqr@IQRequest{iqRequestPayload =
                                   iqb@Element{elementName = en}})
        | nameNamespace en == Just "jabber:iq:roster" -> do
            case iqRequestFrom iqr of
                Just _from -> return [sta] -- Don't handle roster pushes from
                                           -- unauthorized sources
                Nothing -> case unpickleElem xpQuery iqb of
                    Right Query{ queryVer = v
                               , queryItems = [update]
                               } -> do
                        handleUpdate v update
                        _ <- writeStanza sem $ result iqr
                        return []
                    _ -> do
                        errorM "Pontarius.Xmpp" "Invalid roster query"
                        _ <- writeStanza sem $ badRequest iqr
                        return []
    _ -> return [sta]
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
retrieveRoster mbOldRoster sess = do
    useVersioning <- isJust . rosterVer <$> getFeatures sess
    let version = if useVersioning
                then case mbOldRoster of
                      Nothing -> Just ""
                      Just oldRoster -> ver oldRoster
                else Nothing
    res <- sendIQ' Nothing Get Nothing
        (pickleElem xpQuery (Query version []))
        sess
    case res of
        Nothing -> do
            errorM "Pontarius.Xmpp.Roster" "getRoster: sending stanza failed"
            return Nothing
        Just (IQResponseResult (IQResult{iqResultPayload = Just ros}))
            -> case unpickleElem xpQuery ros of
            Left _e -> do
                errorM "Pontarius.Xmpp.Roster" "getRoster: invalid query element"
                return Nothing
            Right ros' -> return . Just $ toRoster ros'
        Just (IQResponseResult (IQResult{iqResultPayload = Nothing})) -> do
            return mbOldRoster
                -- sever indicated that no roster updates are necessary
        Just IQResponseTimeout -> do
            errorM "Pontarius.Xmpp.Roster" "getRoster: request timed out"
            return Nothing
        Just (IQResponseError e) -> do
            errorM "Pontarius.Xmpp.Roster" $ "getRoster: server returned error"
                   ++ show e
            return Nothing
  where
    toRoster (Query v is) = Roster v (Map.fromList
                                             $ map (\i -> (qiJid i, toItem i))
                                               is)

toItem :: QueryItem -> Item
toItem qi = Item { riApproved = fromMaybe False (qiApproved qi)
                 , riAsk = qiAsk qi
                 , riJid = qiJid qi
                 , riName = qiName qi
                 , riSubscription = fromMaybe None (qiSubscription qi)
                 , riGroups = nub $ qiGroups qi
                 }

fromItem :: Item -> QueryItem
fromItem i = QueryItem { qiApproved = Nothing
                       , qiAsk = False
                       , qiJid = riJid i
                       , qiName = riName i
                       , qiSubscription = case riSubscription i of
                           Remove -> Just Remove
                           _ -> Nothing
                       , qiGroups = nub $ riGroups i
                       }

xpItems :: PU [Node] [QueryItem]
xpItems = xpWrap (map (\((app_, ask_, jid_, name_, sub_), groups_) ->
                        QueryItem app_ ask_ jid_ name_ sub_ groups_))
                 (map (\(QueryItem app_ ask_ jid_ name_ sub_ groups_) ->
                        ((app_, ask_, jid_, name_, sub_), groups_))) $
          xpElems "{jabber:iq:roster}item"
          (xp5Tuple
            (xpAttribute' "approved" xpBool)
            (xpWrap isJust
                    (\p -> if p then Just () else Nothing) $
                     xpOption $ xpAttribute_ "ask" "subscribe")
            (xpAttribute  "jid" xpJid)
            (xpAttribute' "name" xpText)
            (xpAttribute' "subscription" xpSubscription)
          )
          (xpFindMatches $ xpElemText "{jabber:iq:roster}group")

xpQuery :: PU [Node] Query
xpQuery = xpWrap (\(ver_, items_) -> Query ver_ items_ )
                 (\(Query ver_ items_) -> (ver_, items_)) $
          xpElem "{jabber:iq:roster}query"
            (xpAttribute' "ver" xpText)
            xpItems

xpSubscription :: PU Text Subscription
xpSubscription = ("xpSubscription", "") <?>
        xpPartial ( \input -> case subscriptionFromText input of
                                   Nothing -> Left "Could not parse subscription."
                                   Just j -> Right j)
                  subscriptionToText
  where
    subscriptionFromText "none" = Just None
    subscriptionFromText "to" = Just To
    subscriptionFromText "from" = Just From
    subscriptionFromText "both" = Just Both
    subscriptionFromText "remove" = Just Remove
    subscriptionFromText _ = Nothing
    subscriptionToText None = "none"
    subscriptionToText To = "to"
    subscriptionToText From = "from"
    subscriptionToText Both = "both"
    subscriptionToText Remove = "remove"
