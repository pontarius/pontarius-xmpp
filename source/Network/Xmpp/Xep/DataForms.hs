{-# LANGUAGE OverloadedStrings #-}

-- | XEP 0004: Data Forms (http://xmpp.org/extensions/xep-0004.html)

module Network.Xmpp.Xep.DataForms where

import qualified Data.Text as Text
import qualified Data.XML.Types as XML

import Data.XML.Pickle
import qualified Data.Text as Text

dataFormNs = "jabber:x:data"

data FormType = FormF | SubmitF | CancelF | ResultF

instance Show FormType where
    show FormF   = "form"
    show SubmitF = "submit"
    show CancelF = "cancel"
    show ResultF = "result"

instance Read FormType where
    readsPrec _ "form"   = [(FormF  , "")]
    readsPrec _ "submit" = [(SubmitF, "")]
    readsPrec _ "cancel" = [(CancelF, "")]
    readsPrec _ "result" = [(ResultF, "")]
    readsPrec _ _        = []

data Option = Option { label :: Text.Text
                     , options :: [Text.Text]
                     }

data Field = Field { fieldType :: FieldType
                   , desc :: Maybe Text.Text
                   , required :: Bool
                   , value :: [Text.Text]
                   , option :: [Option]
                   }


data Form = Form { formType :: FormType
                 , title :: Maybe Text.Text
                 , instructions :: Maybe Text.Text
                 , field :: [Field]
                 }



data FieldType = Boolean
               | Fixed
               | Hidden
               | JidMulti
               | JidSingle
               | ListMulti
               | ListSingle
               | TextMulti
               | TextPrivate
               | TextSingle


instance Show FieldType where
    show Boolean     = "boolean"
    show Fixed       = "fixed"
    show Hidden      = "hidden"
    show JidMulti    = "jid-multi"
    show JidSingle   = "jid-single"
    show ListMulti   = "list-multi"
    show ListSingle  = "list-single"
    show TextMulti   = "text-multi"
    show TextPrivate = "text-private"
    show TextSingle  = "text-single"

instance Read FieldType where
    readsPrec _  "boolean"      = [(Boolean     ,"")]
    readsPrec _  "fixed"        = [(Fixed       ,"")]
    readsPrec _  "hidden"       = [(Hidden      ,"")]
    readsPrec _  "jid-multi"    = [(JidMulti    ,"")]
    readsPrec _  "jid-single"   = [(JidSingle   ,"")]
    readsPrec _  "list-multi"   = [(ListMulti   ,"")]
    readsPrec _  "list-single"  = [(ListSingle  ,"")]
    readsPrec _  "text-multi"   = [(TextMulti   ,"")]
    readsPrec _  "text-private" = [(TextPrivate ,"")]
    readsPrec _  "text-single"  = [(TextSingle  ,"")]
    readsPrec _  _              = []
