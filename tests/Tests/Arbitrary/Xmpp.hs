{-# LANGUAGE TemplateHaskell #-}
module Tests.Arbitrary.Xmpp where

import           Control.Applicative ((<$>), (<*>))
import           Data.Maybe
import qualified Data.Text as Text
import           Network.Xmpp.Types
import           Test.QuickCheck
import           Test.QuickCheck.Instances()
import qualified Text.CharRanges as Ranges
import qualified Text.StringPrep as SP
import qualified Text.StringPrep.Profiles as SP

import           Tests.Arbitrary.Common
import           Tests.Arbitrary.Xml ()

import           Data.Derive.Arbitrary
import           Data.DeriveTH


instance Arbitrary Jid where
    arbitrary = do
        Just jid <- tryJid `suchThat` isJust
        return jid
      where
        tryJid = jidFromTexts <$> maybeGen (genString nodeprepProfile)
                              <*> genString (SP.namePrepProfile False)
                              <*> maybeGen (genString resourceprepProfile)
        maybeGen g = oneof [ return Nothing
                           , Just <$> g
                           ]
        genString profile = Text.pack . take 1024 <$> listOf1 genChar
          where
            genChar = arbitrary `suchThat` (not . isProhibited)
            prohibited = Ranges.toSet $ concat (SP.prohibited profile)
            isProhibited x = Ranges.member x prohibited
                             || x `elem` "@/"

    shrink (Jid lp dp rp) = [ Jid lp' dp rp  | lp' <- shrinkTextMaybe lp]
                         ++ [ Jid lp dp' rp  | dp' <- shrinkText1 dp]
                         ++ [ Jid lp dp  rp' | rp' <- shrinkTextMaybe rp]


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


instance Arbitrary StanzaError where
    arbitrary = StanzaError <$> arbitrary
                            <*> arbitrary
                            <*> oneof [ return Nothing
                                      , Just <$> ((,) <$> arbitrary <*> genText1)
                                      ]
                            <*> arbitrary

-- Auto-derive trivial instances
concat <$> mapM (derive makeArbitrary) [ ''StanzaErrorType
                                       , ''StanzaErrorCondition
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
                                       , ''StreamErrorInfo
                                       -- , ''HandshakeFailed
                                       -- , ''XmppTlsError
--                                       , ''AuthFailure
                                       , ''Version
                                       , ''ConnectionState
                                       , ''TlsBehaviour
                                       ]
