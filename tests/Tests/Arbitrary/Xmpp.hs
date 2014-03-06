{-# LANGUAGE TemplateHaskell #-}
module Tests.Arbitrary.Xmpp where

import           Control.Applicative ((<$>), (<*>))
import           Data.Char
import           Data.Maybe
import qualified Data.Text as Text
import           Network.Xmpp.Internal hiding (elements)
import           Test.QuickCheck
import           Test.QuickCheck.Instances()
import qualified Text.CharRanges as Ranges
import qualified Text.StringPrep as SP
import qualified Text.StringPrep.Profiles as SP

import           Tests.Arbitrary.Common
import           Tests.Arbitrary.Xml ()

import           Data.Derive.Arbitrary
import           Data.DeriveTH


instance Arbitrary NonemptyText where
    arbitrary = Nonempty . Text.pack <$> listOf1
                  (arbitrary `suchThat` (not . isSpace))
    shrink (Nonempty txt) = map Nonempty
                            . filter (not . Text.all isSpace) $ shrink txt

instance Arbitrary Jid where
    arbitrary = do
        Just jid <- tryJid `suchThat` isJust
        return jid
      where
        tryJid = jidFromTexts <$> maybeGen (genString nodeprepProfile)
                              <*> genString (SP.namePrepProfile False)
                              <*> maybeGen (genString resourceprepProfile)

        genString profile = Text.pack . take 1024 <$> listOf1 genChar
          where
            genChar = arbitrary `suchThat` (not . isProhibited)
            prohibited = Ranges.toSet $ concat (SP.prohibited profile)
            isProhibited x = Ranges.member x prohibited
                             || x `elem` "@/"

    shrink (Jid lp dp rp) = [ Jid lp' dp  rp  | lp' <- shrinkMaybe shrink lp]
                         ++ [ Jid lp  dp' rp  | dp' <- shrink dp]
                         ++ [ Jid lp  dp  rp' | rp' <- shrinkMaybe shrink rp]


string :: SP.StringPrepProfile -> Gen [Char]
string profile = take 1024 <$> listOf1 genChar
  where
    genChar = arbitrary `suchThat` (not . isProhibited)
    prohibited = Ranges.toSet $ concat (SP.prohibited profile)
    isProhibited x = Ranges.member x prohibited
                     || x `elem` "@/"

instance Arbitrary LangTag where
    arbitrary = LangTag <$> genTag <*> listOf genTag
        where genTag = fmap Text.pack . listOf1 . elements $ ['a'..'z'] ++ ['A'..'Z']
    shrink (LangTag lt lts) = [LangTag lt' lts | lt' <- shrinkText1 lt] ++
                              [LangTag lt lts' | lts' <- filter (not . Text.null)
                                                         <$> shrink lts]

-- Auto-derive trivial instances
concat <$> mapM (derive makeArbitrary) [ ''StanzaErrorType
                                       , ''StanzaErrorCondition
                                       , ''StanzaError
                                       , ''StreamErrorInfo
                                       , ''IQRequestType
                                       , ''IQRequest
                                       , ''IQResult
                                       , ''IQError
                                       , ''MessageType
                                       , ''Message
                                       , ''MessageError
                                       , ''PresenceType
                                       , ''Presence
                                       , ''PresenceError
                                       , ''Stanza

                                       , ''SaslError
                                       , ''SaslFailure
                                       , ''StreamErrorCondition

                                       -- , ''HandshakeFailed
                                       -- , ''XmppTlsError
--                                       , ''AuthFailure
                                       , ''Version
                                       , ''ConnectionState
                                       , ''TlsBehaviour
                                       ]
