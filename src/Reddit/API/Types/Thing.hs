module Reddit.API.Types.Thing where

import APIBuilder.Query
import Data.Text (Text)

class Thing a where
  fullName :: a -> Text
