module Reddit.API.Types.Options where

import Data.Default

data PaginationOption a = Before a
                        | After a
  deriving (Show, Read, Eq)

data Options a = Options { pagination :: Maybe (PaginationOption a)
                         , limit :: Maybe Int }
  deriving (Show, Read, Eq)

instance Default (Options a) where
  def = Options Nothing Nothing
