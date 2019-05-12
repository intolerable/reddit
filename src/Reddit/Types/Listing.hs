module Reddit.Types.Listing where

import Reddit.Parser

import Control.Applicative
import Data.Aeson
import Data.Traversable
import Data.Monoid
import Network.API.Builder.Query
import Prelude

data ListingType = Hot
                 | New
                 | Rising
                 | Controversial
                 | Top
  deriving (Show, Read, Eq)

instance ToQuery ListingType where
  toQuery k t = return $ (,) k $ case t of
    Hot -> "hot"
    New -> "new"
    Rising -> "rising"
    Controversial -> "controversial"
    Top -> "top"

data Listing t a = Listing { before :: Maybe t
                           , after :: Maybe t
                           , contents :: [a] }
  deriving (Show, Read, Eq)

instance Functor (Listing t) where
  fmap f (Listing b a x) = Listing b a (fmap f x)

instance Ord t => Semigroup (Listing t a) where
  Listing a b cs <> Listing d e fs =
    Listing (max a d) (min b e) (cs <> fs)

instance Ord t => Monoid (Listing t a) where
  mempty = Listing Nothing Nothing []

instance (FromJSON t, FromJSON a) => FromJSON (Listing t a) where
  parseJSON (Object o) = do
    o `ensureKind` "Listing"
    d <- o .: "data"
    Listing <$> (d .:? "before" >>= traverse parseJSON)
            <*> (d .:? "after" >>= traverse parseJSON)
            <*> (o .: "data" >>= (.: "children"))
  parseJSON (String "") = return $ Listing Nothing Nothing []
  parseJSON Null = return $ Listing Nothing Nothing []
  parseJSON _ = mempty
