-- | Miscellaneous utilities for various parts of the library
module Reddit.Utilities
  ( unescape ) where

import Data.Text (Text)
import qualified Data.Text as Text

-- | Quick-'n'-dirty unescaping function for posts / wiki pages etc..
unescape :: Text -> Text
unescape = replace "&gt;" ">" . replace "&lt;" "<" . replace "&amp;" "&"

-- | Swap all instances of a certain string in another string
replace :: Text -- ^ String to replace
        -> Text -- ^ String to replace with
        -> Text -- ^ String to search
        -> Text
replace s r = Text.intercalate r . Text.splitOn s
