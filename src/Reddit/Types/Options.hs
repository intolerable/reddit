module Reddit.Types.Options where

import Data.Default

data PaginationOption a = Before a
                        | After a
  deriving (Show, Read, Eq)

data Options a = Options { pagination :: Maybe (PaginationOption a)
                         , limit :: Maybe Int }
  deriving (Show, Read, Eq)

before :: Options a -> Maybe a
before (Options (Just (Before a)) _) = Just a
before _ = Nothing

after :: Options a -> Maybe a
after (Options (Just (After a)) _) = Just a
after _ = Nothing

instance Default (Options a) where
  def = Options Nothing Nothing
