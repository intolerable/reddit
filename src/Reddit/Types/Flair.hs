module Reddit.Types.Flair where

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

data FlairList = FlairList { flairs :: [Flair]
                           , next :: Maybe UserID
                           , previous :: Maybe UserID }
  deriving (Show, Read, Eq)

instance FromJSON FlairList where
  parseJSON (Object o) =
    FlairList <$> o .: "users"
              <*> o .:? "next"
              <*> o .:? "prev"
  parseJSON _ = mempty
