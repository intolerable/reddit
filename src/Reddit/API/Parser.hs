module Reddit.API.Parser
  ( decode
  , ensureKind ) where

import Control.Monad
import Data.Aeson.Types
import Data.Text (Text)
import Network.API.Builder

ensureKind :: Object -> Text -> Parser ()
ensureKind o k = (o .: "kind") >>= guard . (== k) >> return ()
