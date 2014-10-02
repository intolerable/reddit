module Reddit.API.Types.Listing where

import Reddit.API.Parser

import Control.Applicative
import Data.Aeson
import Data.Monoid (mempty)
import Data.Traversable (traverse)
import Network.API.Builder.Query

data ListingType = Hot
                 | New
                 | Rising
                 | Controversial
                 | Top
  deriving (Show, Read, Eq)

instance ToQuery ListingType where
  toQuery t = Just $ case t of
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
