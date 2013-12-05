{-# OPTIONS_HADDOCK hide #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

-- XEP 0030: Service Discovery (disco)

module Network.Xmpp.Xep.ServiceDiscovery
  ( QueryInfoResult(..)
  , Identity(..)
  , queryInfo
  , Item(..)
  , queryItems
  , DiscoError(..)
  , disco
  )
  where

import           Control.Applicative ((<$>))
import           Control.Monad.Error
import qualified Data.Map as Map
import qualified Data.Text as Text
import           Data.XML.Pickle
import           Data.XML.Types
import           Network.Xmpp
import           Network.Xmpp.Types
import           Network.Xmpp.Concurrent.Types
import           Network.Xmpp.Marshal
import           Network.Xmpp.Plugins
import           Network.Xmpp.Stanza

data DiscoError = DiscoNoQueryElement
                | DiscoIQError (Maybe IQError)
                | DiscoTimeout
                | DiscoXmlError Element UnpickleError

                deriving (Show)

instance Error DiscoError

-- Identity
---------------------

data Identity = Ident { iCategory :: Text.Text
                      , iName     :: Maybe Text.Text
                      , iType     :: Text.Text
                      , iLang     :: Maybe LangTag
                      } deriving Show



data QueryInfoResult = QIR { qiNode       :: Maybe Text.Text
                           , qiIdentities :: [Identity]
                           , qiFeatures :: [Text.Text]
                           } deriving Show


-- | Query an entity for its identity and features
queryInfo :: Maybe Integer -- ^ timeout
          -> Jid -- ^ Entity to query
          -> Maybe Text.Text -- ^ Node
          -> Session
          -> IO (Either DiscoError QueryInfoResult)
queryInfo timeout to' node context = do
    res <- sendIQ' timeout (Just to') Get Nothing queryBody context
    return $ case fst <$> res of
        Left _e -> Left $ DiscoIQError Nothing
        Right (IQResponseError e) -> Left $ DiscoIQError (Just e)
        Right (IQResponseResult r) -> case iqResultPayload r of
            Nothing -> Left DiscoNoQueryElement
            Just p -> case unpickleElem xpQueryInfo p of
                Left e -> Left $ DiscoXmlError p e
                Right r' -> Right r'
  where
    queryBody = pickleElem xpQueryInfo (QIR node [] [])


handleInfoRequest
  :: [Identity]
     -> [Text.Text]
     -> Map.Map Text.Text ([Identity], [Text.Text])
     -> (Stanza -> IO (Either XmppFailure ()) )
     -> Stanza
     -> [Annotation]
     -> IO [Annotated Stanza]
handleInfoRequest ids fs infoNodes =
    handleIQRequest Get pickler $ \iqr (QIR node _ _) _ ->
    return . fmap (\x -> (Just $ pickle (xpRoot $ pickler) x, [])) $
    case node of
        Nothing -> Right . QIR node ids $ addDisco fs
        Just n -> case Map.lookup n infoNodes of
            Nothing -> Left $ iqError ItemNotFound iqr
            Just (ids', fs') -> Right . QIR node ids' $ addDisco fs'
  where pickler = xpUnliftElems xpQueryInfo
        addDisco x = "http://jabber.org/protocol/disco#info"
                   : "http://jabber.org/protocol/disco#items"
                   : x


-- Items
--------------------------

data Item = Item { itemJid :: Jid
                 , itemName :: Maybe Text.Text
                 , itemNode :: Maybe Text.Text
                 } deriving Show

-- | Query an entity for Items of a node
queryItems :: Maybe Integer -- ^ Timeout
           -> Jid -- ^ Entity to query
           -> Maybe Text.Text -- ^ Node
           -> Session
           -> IO (Either DiscoError (Maybe Text.Text, [Item]))
queryItems timeout to' node session' = do
    res <- sendIQ' timeout (Just to') Get Nothing queryBody session'
    return $ case fst <$> res of
        Left _ -> Left $ DiscoIQError Nothing
        Right (IQResponseError e) -> Left $ DiscoIQError (Just e)
        Right (IQResponseResult r) -> case iqResultPayload r of
            Nothing -> Left DiscoNoQueryElement
            Just p -> case unpickleElem xpQueryItems p of
                Left e -> Left $ DiscoXmlError p e
                Right r' -> Right r'
  where
    queryBody = pickleElem xpQueryItems (node, [])


handleItemsRequest :: (Maybe Text.Text -> IO (Maybe [Item]))
                   -> (Stanza -> IO (Either XmppFailure ()))
                   -> Stanza
                   -> [Annotation]
                   -> IO [Annotated Stanza]
handleItemsRequest getItems = handleIQRequest Get pickler $ \iqr (node, _) _ -> do
    mbIs <- getItems node
    case mbIs of
        Nothing -> return . Left $ iqError ItemNotFound iqr
        Just is -> return $ Right ( Just $ pickle (xpRoot pickler) (node, is)
                                  , []
                                  )

  where
    pickler = xpUnliftElems xpQueryItems

-----------------------
-- Picklers -----------
-----------------------

discoInfoNS :: Text.Text
discoInfoNS = "http://jabber.org/protocol/disco#info"

infoN :: Text.Text -> Name
infoN name = Name name (Just discoInfoNS) Nothing

xpIdentities :: PU [Node] [Identity]
xpIdentities = xpWrap (map $(\(cat, n, tp, l) -> Ident cat n tp l) . fst)
               (map $ \(Ident cat n tp l) -> ((cat, n, tp, l),())) $
              xpElems (infoN "identity")
               (xp4Tuple
                  (xpAttr        "category" xpText)
                  (xpAttrImplied "name"     xpText)
                  (xpAttr        "type"     xpText)
                  xpLangTag
               )
               xpUnit

xpFeatures :: PU [Node] [Text.Text]
xpFeatures = xpWrap (map fst) (map (,())) $
              xpElems (infoN "feature")
                (xpAttr "var" xpText)
                xpUnit

xpQueryInfo :: PU [Node] QueryInfoResult
xpQueryInfo = xpWrap (\(nd, (feats, ids)) -> QIR nd ids feats)
                     (\(QIR nd ids feats) -> (nd, (feats, ids))) $
              xpElem (infoN "query")
                (xpAttrImplied "node" xpText)
                (xp2Tuple
                     xpFeatures
                     xpIdentities
                     )

discoItemsNS :: Text.Text
discoItemsNS = "http://jabber.org/protocol/disco#items"

itemsN :: Text.Text -> Name
itemsN n = Name n (Just discoItemsNS) Nothing

xpItem :: PU [Node] Item
xpItem = xpWrap (\(jid, name, node) -> Item jid name node)
                (\(Item jid name node) -> (jid, name, node)) $
         xpElemAttrs (itemsN "item")
           (xp3Tuple
              (xpAttr "jid" xpJid)
              (xpAttrImplied "name" xpText)
              (xpAttrImplied "node" xpText))


xpQueryItems :: PU [Node] (Maybe Text.Text, [Item])
xpQueryItems = xpElem (itemsN "query")
                 (xpAttrImplied "node" xpText)
                 (xpAll xpItem)


disco :: [Identity]
      -> [Text.Text]
      -> Map.Map Text.Text ([Identity], [Text.Text])
      -> (Maybe Text.Text -> IO (Maybe [Item]))
      -> Plugin
disco ids fs ins items out = return $ Plugin'
                       { inHandler = \sta as -> do
                              res <- handleInfoRequest ids fs ins out sta as
                              concat <$>
                                forM res (uncurry $ handleItemsRequest items out)
                       , outHandler = out
                       , onSessionUp = const $ return ()
                       }
