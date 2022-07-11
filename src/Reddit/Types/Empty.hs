module Reddit.Types.Empty ( nothing ) where

import Control.Monad (liftM)
import Data.Aeson
import Data.Aeson.Types
import Data.Monoid
import Prelude
import qualified Data.Aeson.KeyMap as KeyMap

-- | More specific @void@ for forcing a @Empty@ @FromJSON@ instance
nothing :: Monad m => m Empty -> m ()
nothing = liftM $ const ()

data Empty = Empty
  deriving (Show, Read, Eq)

instance FromJSON Empty where
  parseJSON (Object o) =
    if KeyMap.null o
      then return Empty
      else do
        errs <- (o .: "json") >>= (.: "errors") :: Parser [Value]
        if null errs
          then return Empty
          else mempty
  parseJSON _ = mempty
