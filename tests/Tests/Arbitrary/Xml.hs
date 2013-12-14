module Tests.Arbitrary.Xml where

import           Control.Applicative ((<$>), (<*>))
import           Test.QuickCheck
import           Test.QuickCheck.Instances()
-- import Data.DeriveTH
import qualified Data.Text as Text
import           Data.XML.Types
import           Tests.Arbitrary.Common
import           Text.CharRanges


selectFromRange :: Range -> Gen Char
selectFromRange (Single a) = return a
selectFromRange (Range a b) = choose (a, b)

nameStartChar :: [Range]
nameStartChar =
    [ -- Single ':'
      Single '_'
    , Range 'A' 'Z'
    , Range 'a' 'z'
    , Range '\xC0' '\xD6'
    , Range '\xD8' '\xF6'
    , Range '\xF8' '\x2FF'
    , Range '\x370' '\x37D'
    , Range '\x37F' '\x1FFF'
    , Range '\x200C' '\x200D'
    , Range '\x2070' '\x218F'
    , Range '\x2C00' '\x2FEF'
    , Range '\x3001' '\xD7FF'
    , Range '\xF900' '\xFDCF'
    , Range '\xFDF0' '\xFFFD'
    , Range '\x10000' '\xEFFFF'
    ]

nameChar :: [Range]
nameChar =
      Single '-'
    : Single '.'
    : Single '\xB7'
    : Range '0' '9'
    : Range '\x0300' '\x036F'
    : Range '\x203F' '\x2040'
    : nameStartChar


genNCName :: Gen Text.Text
genNCName = do
    sc <- elements nameStartChar >>= selectFromRange
    ncs <- listOf $ elements nameChar >>= selectFromRange
    return . Text.pack $ sc:ncs

-- | Cap the size of child elements.
slow :: Gen a -> Gen a
slow g = sized $ \n -> resize (min 5 (n `div` 4))  g

instance Arbitrary Name where
    arbitrary = Name <$> genNCName <*> genMaybe genNCName <*> genMaybe genNCName
      where
        genMaybe g = oneof [return Nothing, Just <$> g]
    shrink (Name a b c) = [ Name a' b c | a' <- shrinkText1 a]
                        ++[ Name a b' c | b' <- shrinkTextMaybe b]
                        ++[ Name a b c' | c' <- shrinkTextMaybe c]

instance Arbitrary Content where
    arbitrary = ContentText <$> arbitrary
    shrink (ContentText txt) = ContentText <$> shrinkText1 txt
    shrink _ = []


instance Arbitrary Node where
    arbitrary = oneof [ NodeElement <$> arbitrary
                      , NodeContent <$> arbitrary
                      ]
    shrink (NodeElement e) = NodeElement <$> shrink e
    shrink (NodeContent c) = NodeContent <$> shrink c
    shrink _ = []

instance Arbitrary Element where
    arbitrary = Element <$> arbitrary <*> slow arbitrary <*> slow arbitrary
    shrink (Element a b c) =
          [ Element a' b c | a' <- shrink a]
        ++[ Element a b' c | b' <- shrink b]
        ++[ Element a b c' | c' <- shrink c]
