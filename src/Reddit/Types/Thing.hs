module Reddit.Types.Thing where

import Data.Text (Text)

class Thing a where
  fullName :: a -> Text
