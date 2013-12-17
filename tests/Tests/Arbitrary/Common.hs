module Tests.Arbitrary.Common where

import           Control.Applicative ((<$>))
import           Data.Char
import qualified Data.Text as Text
import           Test.QuickCheck
import           Test.QuickCheck.Instances ()

shrinkText1 :: Text.Text -> [Text.Text]
shrinkText1 txt = filter (not . Text.null) $ shrink txt

shrinkTextMaybe :: Maybe Text.Text -> [Maybe Text.Text]
shrinkTextMaybe mbtxt = filter (\mb -> mb /= Just (Text.empty)) $ shrink mbtxt

genText1 :: Gen Text.Text
genText1 = Text.pack <$> string1
  where
    string1 = listOf1 arbitrary `suchThat` (not . all isSpace)

maybeGen :: Gen a -> Gen (Maybe a)
maybeGen g = oneof [ return Nothing
                   , Just <$> g
                   ]

shrinkMaybe :: (t -> [t]) -> Maybe t -> [Maybe t]
shrinkMaybe _s Nothing = []
shrinkMaybe s (Just x) = Nothing : map Just (s x)
