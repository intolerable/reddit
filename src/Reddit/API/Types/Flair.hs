module Reddit.API.Types.Flair where

import Reddit.API.Types.User

import Control.Applicative
import Data.Aeson
import Data.Monoid
import Data.Text (Text)

data Flair = Flair { user :: Username
                   , text :: Maybe  Text
                   , cssClass :: Maybe Text }
  deriving (Show, Read, Eq)

instance FromJSON Flair where
  parseJSON (Object o) =
    Flair <$> (Username <$> o .: "user")
          <*> o .:? "flair_text"
          <*> o .:? "flair_css_class"
  parseJSON _ = mempty

data FlairList = FlairList { flairs :: [Flair]
                           , next :: Maybe Text
                           , previous :: Maybe Text }
  deriving (Show, Read, Eq)

instance FromJSON FlairList where
  parseJSON (Object o) =
    FlairList <$> o .: "users"
              <*> o .:? "next"
              <*> o .:? "prev"
  parseJSON _ = mempty
