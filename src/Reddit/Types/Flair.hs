module Reddit.Types.Flair where

import Reddit.Types.Listing
import Reddit.Types.User

import Control.Applicative
import Data.Aeson
import Data.Monoid
import Data.Text (Text)
import Prelude


data Flair = Flair { user :: Username
                   , text :: Maybe Text
                   , cssClass :: Maybe Text }
  deriving (Show, Read, Eq)

instance FromJSON Flair where
  parseJSON (Object o) =
    Flair <$> o .: "user"
          <*> o .:? "flair_text"
          <*> o .:? "flair_css_class"
  parseJSON _ = mempty

data FList = FList [Flair] (Maybe UserID) (Maybe UserID)
  deriving (Show, Read, Eq)

instance FromJSON FList where
  parseJSON (Object o) =
    FList <$> o .: "users"
          <*> o .:? "next"
          <*> o .:? "prev"
  parseJSON _ = mempty

type FlairListing = Listing UserID Flair

flistToListing :: FList -> FlairListing
flistToListing (FList f b a) = Listing b a f
