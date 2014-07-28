module Reddit.API.Types.Options where

import Data.Default

data Options a = Options { after :: Maybe a
                         , before :: Maybe a
                         , limit :: Maybe Int }
  deriving (Show, Read, Eq)

instance Default (Options a) where
  def = Options Nothing Nothing Nothing
