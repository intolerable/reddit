module Reddit.API.Parser
  ( decode
  , ensureKind ) where

import Control.Monad (guard)
import Data.Aeson.Types (Parser, Object, (.:))
import Data.Text (Text)
import Network.API.Builder (decode)

ensureKind :: Object -> Text -> Parser ()
ensureKind o k = do
  kind <- o .: "kind"
  guard $ kind == k
