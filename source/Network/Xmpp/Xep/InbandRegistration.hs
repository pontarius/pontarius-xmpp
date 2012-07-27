-- | XEP 0077: In-Band Registration
-- http://xmpp.org/extensions/xep-0077.html

{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.Xmpp.Xep.InbandRegistration where

import           Control.Applicative((<$>))
import           Control.Arrow(left)
import           Control.Exception
import           Control.Monad.Error
import           Control.Monad.State

import           Data.Either (partitionEithers)
import qualified Data.Text as Text
import           Data.XML.Pickle
import qualified Data.XML.Types as XML

import           Network.Xmpp.Monad
import           Network.Xmpp.Pickle
import           Network.Xmpp.Types


-- In-Band Registration name space
ibrns :: Text.Text
ibrns = "jabber:iq:register"

ibrName x = (XML.Name x (Just ibrns) Nothing)

data Query = Query { instructions :: Maybe Text.Text
                   , registered   :: Bool
                   , fields       ::[(Field, Maybe Text.Text)]
                   } deriving Show

emptyQuery = Query Nothing False []

supported = do
    fs <- other <$> gets sFeatures
    let fe = XML.Element "{http://jabber.org/features/iq-register}register" [] []
    return $ fe `elem` fs


data IbrError = IbrNotSupported
              | IbrIQError IQError
                deriving (Show)
instance Error IbrError

query :: Query -> XmppConMonad (Either IbrError Query)
query x = do
    answer <- xmppSendIQ' "ibr" Nothing Get Nothing (pickleElem xpQuery x)
    case answer of
        Right IQResult{iqResultPayload = Just b} ->
            case unpickleElem xpQuery b of
                Right query -> return $ Right query
                Left e -> throw . StreamXMLError $
                            "RequestField: unpickle failed, got "
                            ++ Text.unpack (ppUnpickleError e)
                            ++ " saw " ++ ppElement b
        Left e -> return . Left $ IbrIQError e

data RegisterError = IbrError IbrError
                   | MissingFields   [Field]
                   | AlreadyRegistered
                     deriving (Show)

instance Error RegisterError

mapError f = mapErrorT (liftM $ left f)

registerWith :: [(Field, Text.Text)] -> XmppConMonad (Either RegisterError Query)
registerWith givenFields = runErrorT $ do
    fs <- mapError IbrError $ ErrorT requestFields
    when (registered fs) . throwError $ AlreadyRegistered
    let res = flip map (fields fs) $ \(field,_) ->
            case lookup field givenFields of
                Just entry -> Right (field, Just entry)
                Nothing -> Left field
    fields <- case partitionEithers res of
        ([],fs) -> return fs
        (fs,_) -> throwError $ MissingFields fs
    result <- mapError IbrError . ErrorT .  query $ emptyQuery {fields}
    return result

requestFields = runErrorT $ do
    supp <- supported
    unless supp $ throwError $ IbrNotSupported
    qr <- ErrorT $ query emptyQuery
    return $ qr

xpQuery :: PU [XML.Node] Query
xpQuery = xpWrap
            (\(is, r, fs) -> Query is r fs)
            (\(Query is r fs) -> (is, r, fs)) $
            xpElemNodes (ibrName "query") $
              xp3Tuple
                 (xpOption $
                    xpElemNodes (ibrName "instructions") (xpContent $ xpText))
                 (xpElemExists (ibrName "registered"))
                 (xpAllByNamespace ibrns  ( xpWrap
                                              (\(name,_,c) -> (name, c))
                                              (\(name,c) -> (name,(),c)) $
                         xpElemByNamespace ibrns xpPrim xpUnit
                           (xpOption $ xpContent xpText)
                        ))

data Field = Username
           | Nick
           | Password
           | Name
           | First
           | Last
           | Email
           | Address
           | City
           | State
           | Zip
           | Phone
           | Url
           | Date
           | Misc
           | Text
           | Key
           | OtherField Text.Text
             deriving Eq

instance Show Field where
    show  Username       = "username"
    show  Nick           = "nick"
    show  Password       = "password"
    show  Name           = "name"
    show  First          = "first"
    show  Last           = "last"
    show  Email          = "email"
    show  Address        = "address"
    show  City           = "city"
    show  State          = "state"
    show  Zip            = "zip"
    show  Phone          = "phone"
    show  Url            = "url"
    show  Date           = "date"
    show  Misc           = "misc"
    show  Text           = "text"
    show  Key            = "key"
    show  (OtherField x) = Text.unpack x

instance Read Field where
    readsPrec _ "username" = [(Username     , "")]
    readsPrec _ "nick"     = [(Nick         , "")]
    readsPrec _ "password" = [(Password     , "")]
    readsPrec _ "name"     = [(Name         , "")]
    readsPrec _ "first"    = [(First        , "")]
    readsPrec _ "last"     = [(Last         , "")]
    readsPrec _ "email"    = [(Email        , "")]
    readsPrec _ "address"  = [(Address      , "")]
    readsPrec _ "city"     = [(City         , "")]
    readsPrec _ "state"    = [(State        , "")]
    readsPrec _ "zip"      = [(Zip          , "")]
    readsPrec _ "phone"    = [(Phone        , "")]
    readsPrec _ "url"      = [(Url          , "")]
    readsPrec _ "date"     = [(Date         , "")]
    readsPrec _ "misc"     = [(Misc         , "")]
    readsPrec _ "text"     = [(Text         , "")]
    readsPrec _ "key"      = [(Key          , "")]
    readsPrec _ x          = [(OtherField $ Text.pack x , "")]



-- Registered
-- Instructions