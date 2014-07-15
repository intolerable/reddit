module Reddit.API.Types.Empty where

import Control.Monad (void)
import Data.Aeson
import Data.Aeson.Types
import Data.Monoid (mempty)
import qualified Data.HashMap.Strict as Hash

nothing :: Functor m => m Empty -> m ()
nothing = void

data Empty = Empty
  deriving (Show, Read, Eq)

instance FromJSON Empty where
  parseJSON (Object o) =
    if Hash.null o
      then return Empty
      else do
        errs <- (o .: "json") >>= (.: "errors") :: Parser [Value]
        if null errs
          then return Empty
          else mempty
  parseJSON _ = mempty
