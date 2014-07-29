module Reddit.API.Types.Listing where

import Reddit.API.Parser

import Control.Applicative
import Data.Aeson
import Data.Monoid (mempty)

data Listing a = Listing [a]
  deriving (Show, Read, Eq)

instance Functor Listing where
  fmap f (Listing a) = Listing (fmap f a)

instance FromJSON a => FromJSON (Listing a) where
  parseJSON (Object o) = do
    o `ensureKind` "Listing"
    Listing <$> (o .: "data" >>= (.: "children"))
  parseJSON (String "") = return $ Listing []
  parseJSON Null = return $ Listing []
  parseJSON _ = mempty

data ListingOptions a = ListingOptions { before :: Maybe a
                                       , after :: Maybe a
                                       , count :: Integer }
  deriving (Show, Read, Eq)
