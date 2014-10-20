module Reddit.API.Parser
  ( decode
  , ensureKind
  , stripPrefix ) where

import Control.Monad (guard)
import Data.Aeson.Types (Parser, Object, (.:))
import Data.Monoid
import Data.Text (Text)
import Network.API.Builder (decode)
import qualified Data.Text as Text

ensureKind :: Object -> Text -> Parser ()
ensureKind o k = do
  kind <- o .: "kind"
  guard $ kind == k

stripPrefix :: Text -> Text -> Parser Text
stripPrefix prefix string =
  case Text.breakOn "_" string of
    (t, i) | t == prefix ->
      case Text.stripPrefix "_" i of
        Just rest -> return rest
        Nothing -> mempty
    (i, "") -> return i
    _ -> mempty
