module Reddit.API.Types.Subreddit where

import Reddit.API.Parser
import Reddit.API.Types.Thing

import Control.Applicative
import Data.Aeson
import Data.Monoid
import Data.Text (Text)
import Network.API.Builder.Query

newtype SubredditName = R Text
  deriving (Show, Read, Eq)

instance ToQuery SubredditName where
  toQuery (R sub) = Just sub

newtype SubredditID = SubredditID Text
  deriving (Show, Read, Eq)

instance FromJSON SubredditID where
  parseJSON j = SubredditID <$> parseJSON j

instance Thing SubredditID where
  fullName (SubredditID i) = mconcat [subredditPrefix, "_", i]

instance ToQuery SubredditID where
  toQuery = Just . fullName

data Subreddit = Subreddit { subredditID :: SubredditID
                           , name :: SubredditName
                           , title :: Text
                           , subscribers :: Integer
                           , userIsBanned :: Bool } deriving (Show, Eq)

instance FromJSON Subreddit where
  parseJSON (Object o) = do
    o `ensureKind` subredditPrefix
    d <- o .: "data"
    Subreddit <$> d .: "id"
              <*> (R <$> d .: "display_name")
              <*> d .: "title"
              <*> d .: "subscribers"
              <*> d .: "user_is_banned"
  parseJSON _ = mempty

instance Thing Subreddit where
  fullName sub = mconcat [subredditPrefix, "_", s]
    where SubredditID s = subredditID sub

subredditPrefix :: Text
subredditPrefix = "t5"
