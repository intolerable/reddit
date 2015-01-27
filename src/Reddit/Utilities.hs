module Reddit.Utilities where

import Data.Text (Text)
import qualified Data.Text as Text

unescape :: Text -> Text
unescape = replace "&gt;" ">" . replace "&lt;" "<" . replace "&amp;" "&"
  where replace s r = Text.intercalate r . Text.splitOn s
