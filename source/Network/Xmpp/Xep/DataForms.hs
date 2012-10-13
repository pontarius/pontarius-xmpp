{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}

-- | XEP 0004: Data Forms (http://xmpp.org/extensions/xep-0004.html)

module Network.Xmpp.Xep.DataForms where

import qualified Data.Text as Text
import qualified Data.XML.Types as XML

import Data.XML.Pickle
import qualified Data.Text as Text

import qualified Text.XML.Stream.Parse as Parse

dataFormNs :: Text.Text
dataFormNs = "jabber:x:data"

dataFormName :: Text.Text -> XML.Name
dataFormName n = XML.Name n (Just dataFormNs) Nothing

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

data Option = Option { label :: Maybe Text.Text
                     , options :: [Text.Text]
                     } deriving Show

data Field = Field { fieldVar :: Maybe Text.Text
                   , fieldLabel :: Maybe Text.Text
                   , fieldType :: Maybe FieldType
                   , fieldDesc :: Maybe Text.Text
                   , fieldRequired :: Bool
                   , fieldValues :: [Text.Text]
                   , fieldOptions :: [Option]
                   } deriving Show


data Form = Form { formType :: FormType
                 , title :: Maybe Text.Text
                 , instructions :: [Text.Text]
                 , fields :: [Field]
                 , reported :: Maybe [Field]
                 , items :: [[Field]]
                 } deriving Show

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


xpForm :: PU [XML.Node] Form
xpForm = xpWrap (\(tp , (title, instructions, fields, reported, items)) ->
                  Form tp title (map snd instructions) fields reported (map snd items))
                 (\(Form tp title instructions fields reported items) ->
                     (tp ,
                     (title, map ((),) instructions
                     , fields, reported, map ((),) items)))

          $
    xpElem (dataFormName "x")
      (xpAttr "type" xpPrim)
  (xp5Tuple
      (xpOption $ xpElemNodes (dataFormName "title") (xpContent xpId))
      (xpElems (dataFormName "instructions") xpUnit (xpContent xpId))
      xpFields
      (xpOption $ xpElemNodes (dataFormName "reported") xpFields)
      (xpElems (dataFormName "item") xpUnit xpFields))

xpFields :: PU [XML.Node] [Field]
xpFields = xpWrap (map $ \((var, tp, label),(desc, req, vals, opts))
                     -> Field var label tp desc req vals opts)
                  (map $ \(Field var label tp desc req vals opts)
                        -> ((var, tp, label),(desc, req, vals, opts))) $
    xpElems (dataFormName "field")
     (xp3Tuple
       (xpAttrImplied "var"  xpId )
       (xpAttrImplied "type" xpPrim )
       (xpAttrImplied "label" xpId )
     )
     (xp4Tuple
       (xpOption $ xpElemText (dataFormName "desc"))
       (xpElemExists (dataFormName "required"))
       xpValues
       xpOptions )

xpValues :: PU [XML.Node] [Text.Text]
xpValues  = xpWrap (map snd) (map ((),))
                    (xpElems (dataFormName "value") xpUnit (xpContent xpId))

xpOptions :: PU [XML.Node] [Option]
xpOptions = xpWrap
              (map $ \(l, os) -> Option l os)
              (map $ \(Option l os) -> (l, os)) $
            xpElems (dataFormName "option")
               (xpAttrImplied "label" xpId)
               xpValues
