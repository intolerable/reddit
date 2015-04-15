-- | Utilities for parsing JSON responses from the API
module Reddit.Parser
  ( ensureKind
  , stripPrefix ) where

import Control.Monad (guard)
import Data.Aeson.Types (Parser, Object, (.:))
import Data.Monoid
import Data.Text (Text)
import Prelude
import qualified Data.Text as Text

-- | Fail to parse unless the @Object@'s kind is what it should be.
ensureKind :: Object -> Text -> Parser ()
ensureKind o k = do
  kind <- o .: "kind"
  guard $ kind == k

-- | Parse an ID in either the "tX_XXXXXX" or simply "XXXXXX" format.
stripPrefix :: Text -> Text -> Parser Text
stripPrefix prefix string =
  case Text.breakOn "_" string of
    (t, i) | t == prefix ->
      case Text.stripPrefix "_" i of
        Just rest -> return rest
        Nothing -> mempty
    (i, "") -> return i
    _ -> mempty
