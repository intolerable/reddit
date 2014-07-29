module Reddit.API.Types.Subreddit where

import Reddit.API.Parser
import Reddit.API.Types.Thing

import APIBuilder.Query
import Control.Applicative
import Data.Aeson
import Data.Monoid (mempty)
import Data.Text (Text)
import qualified Data.Text as T

newtype SubredditName = R T.Text
  deriving (Show, Read, Eq)

newtype SubredditID = SubredditID T.Text
  deriving (Show, Read, Eq)

instance Thing SubredditID where
  fullName (SubredditID i) = T.concat [subredditPrefix, "_", i]

instance ToQuery SubredditID where
  toQuery = Just . fullName

data Subreddit = Subreddit { subredditID :: T.Text
                           , name :: SubredditName
                           , title :: T.Text
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
  fullName s = T.concat [subredditPrefix, "_", subredditID s]

subredditPrefix :: Text
subredditPrefix = "t5"
