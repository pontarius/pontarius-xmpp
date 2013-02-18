{-# OPTIONS_HADDOCK hide #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

-- XEP 0030: Service Discovery (disco)

module Network.Xmpp.Xep.ServiceDiscovery
  ( QueryInfoResult(..)
  , Identity(..)
  , queryInfo
  , xmppQueryInfo
  , Item
  , queryItems
  , DiscoError(..)
  )
  where

import           Control.Applicative((<$>))
import           Control.Monad.IO.Class
import           Control.Monad.Error

import qualified Data.Text as Text
import           Data.XML.Pickle
import           Data.XML.Types

import           Network.Xmpp
import           Network.Xmpp.Internal
import           Control.Concurrent.STM.TMVar

data DiscoError = DiscoNoQueryElement
                | DiscoIQError (Maybe IQError)
                | DiscoTimeout
                | DiscoXmlError Element UnpickleError

                deriving (Show)

instance Error DiscoError

data Identity = Ident { iCategory :: Text.Text
                      , iName     :: Maybe Text.Text
                      , iType     :: Text.Text
                      , iLang     :: Maybe LangTag
                      } deriving Show

data QueryInfoResult = QIR { qiNode       :: Maybe Text.Text
                           , qiIdentities :: [Identity]
                           , qiFeatures :: [Text.Text]
                           } deriving Show

discoInfoNS :: Text.Text
discoInfoNS = "http://jabber.org/protocol/disco#info"

infoN :: Text.Text -> Name
infoN name = (Name name (Just discoInfoNS) Nothing)

xpIdentities = xpWrap (map $(\(cat, n, tp, lang) -> Ident cat n tp lang) . fst)
               (map $ \(Ident cat n tp lang) -> ((cat, n, tp, lang),())) $
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

xpQueryInfo = xpWrap (\(nd, (feats, ids)) -> QIR nd ids feats)
                     (\(QIR nd ids feats) -> (nd, (feats, ids))) $
              xpElem (infoN "query")
                (xpAttrImplied "node" xpText)
                (xp2Tuple
                     xpFeatures
                     xpIdentities
                     )

-- | Query an entity for it's identity and features
queryInfo :: Jid -- ^ Entity to query
          -> Maybe Text.Text -- ^ Node
          -> Session
          -> IO (Either DiscoError QueryInfoResult)
queryInfo to node context = do
    res <- sendIQ' (Just to) Get Nothing queryBody context
    return $ case res of
        IQResponseError e -> Left $ DiscoIQError (Just e)
        IQResponseTimeout -> Left $ DiscoTimeout
        IQResponseResult r -> case iqResultPayload r of
            Nothing -> Left DiscoNoQueryElement
            Just p -> case unpickleElem xpQueryInfo p of
                Left e -> Left $ DiscoXmlError p e
                Right r -> Right r
  where
    queryBody = pickleElem xpQueryInfo (QIR node [] [])


xmppQueryInfo :: Maybe Jid
     -> Maybe Text.Text
     -> TMVar Stream
     -> IO (Either DiscoError QueryInfoResult)
xmppQueryInfo to node con = do
    res <- pushIQ "info" to Get Nothing queryBody con
    return $ case res of
        Left e -> Left $ DiscoIQError Nothing
        Right res' -> case res' of
            Left e -> Left $ DiscoIQError (Just e)
            Right r -> case iqResultPayload r of
                Nothing -> Left DiscoNoQueryElement
                Just p -> case unpickleElem xpQueryInfo p of
                    Left e -> Left $ DiscoXmlError p e
                    Right r -> Right r
  where
    queryBody = pickleElem xpQueryInfo (QIR node [] [])


--
-- Items
--

data Item = Item { itemJid :: Jid
                 , itemName :: Maybe Text.Text
                 , itemNode :: Maybe Text.Text
                 } deriving Show

discoItemsNS :: Text.Text
discoItemsNS = "http://jabber.org/protocol/disco#items"

itemsN :: Text.Text -> Name
itemsN n = Name n (Just discoItemsNS) Nothing

xpItem = xpWrap (\(jid, name, node) -> Item jid name node)
                (\(Item jid name node) -> (jid, name, node)) $
         xpElemAttrs (itemsN "item")
           (xp3Tuple
              (xpAttr "jid" xpPrim)
              (xpAttrImplied "name" xpText)
              (xpAttrImplied "node" xpText))


xpQueryItems = xpElem (itemsN "query")
                 (xpAttrImplied "node" xpText)
                 (xpAll xpItem)

-- | Query an entity for Items of a node
queryItems :: Jid -- ^ Entity to query
           -> Maybe Text.Text -- ^ Node
           -> Session
           -> IO (Either DiscoError (Maybe Text.Text, [Item]))
queryItems to node session = do
    res <- sendIQ' (Just to) Get Nothing queryBody session
    return $ case res of
        IQResponseError e -> Left $ DiscoIQError (Just e)
        IQResponseTimeout -> Left $ DiscoTimeout
        IQResponseResult r -> case iqResultPayload r of
            Nothing -> Left DiscoNoQueryElement
            Just p -> case unpickleElem xpQueryItems p of
                Left e -> Left $ DiscoXmlError p e
                Right r -> Right r
  where
    queryBody = pickleElem xpQueryItems (node, [])

-- Given a pickler and an object, produces an Element.
pickleElem :: PU [Node] a -> a -> Element
pickleElem p = pickle $ xpNodeElem p

-- Given a pickler and an element, produces an object.
unpickleElem :: PU [Node] a -> Element -> Either UnpickleError a
unpickleElem p x = unpickle (xpNodeElem p) x

xpNodeElem :: PU [Node] a -> PU Element a
xpNodeElem xp = PU { pickleTree = \x -> Prelude.head $ (pickleTree xp x) >>= \y ->
                      case y of
                        NodeElement e -> [e]
                        _ -> []
             , unpickleTree = \x -> case unpickleTree xp $ [NodeElement x] of
                        Left l -> Left l
                        Right (a,(_,c)) -> Right (a,(Nothing,c))
                   }

xpLangTag :: PU [Attribute] (Maybe LangTag)
xpLangTag = xpAttrImplied xmlLang xpPrim

xmlLang :: Name
xmlLang = Name "lang" (Just "http://www.w3.org/XML/1998/namespace") (Just "xml")
