module Common.SimpleTypes (
    String50,
    String50ValidationError(..),
    makeString50,
    makeMaybeString50,
) where

import           Data.Text    (Text)
import qualified Data.Text    as Text

-- String50
data String50ValidationError =
    TooLong Text | EmptyString Text deriving(Eq, Show)
newtype String50 = String50 Text deriving (Eq, Show)

makeString50 :: Text -> Either String50ValidationError String50
makeString50 s | Text.length s > 50 = Left $ TooLong s
             | Text.length s == 0 = Left $ EmptyString s
             | otherwise = Right $ String50 s

makeMaybeString50 :: Text -> Maybe String50
makeMaybeString50 s | Text.length s == 0 = Nothing
                  | otherwise = Just $ String50 s

getString50Value :: String50 -> Text
getString50Value (String50 v) = v