module Reddit.API.Types.Wiki where

import Data.Text (Text)

newtype RevisionID = RevisionID Text
  deriving (Show, Read, Eq)
