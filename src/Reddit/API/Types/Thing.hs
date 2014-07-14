module Reddit.API.Types.Thing where

import Data.Text (Text)
import APIBuilder.Query

class Thing a where
  fullName :: a -> Text
