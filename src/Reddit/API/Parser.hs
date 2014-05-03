module Reddit.API.Parser
  ( decode
  , ensureKind ) where

import APIBuilder
import Control.Monad
import Data.Aeson.Types
import Data.Text (Text)

ensureKind :: Object -> Text -> Parser ()
ensureKind o k = (o .: "kind") >>= guard . (== k) >> return ()
