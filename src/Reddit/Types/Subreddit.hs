module Reddit.Types.Subreddit where

import Reddit.Parser
import Reddit.Types.Thing

import Control.Applicative
import Data.Aeson
import Data.Monoid
import Data.Text (Text)
import Network.API.Builder.Query
import Prelude
import qualified Data.Text as Text

newtype SubredditName = R Text
  deriving (Show, Read)

instance Eq SubredditName where
  R x == R y = Text.toLower x == Text.toLower y

instance ToQuery SubredditName where
  toQuery k (R sub) = [(k, sub)]

instance FromJSON SubredditName where
  parseJSON j = R <$> parseJSON j

newtype SubredditID = SubredditID Text
  deriving (Show, Read, Eq)

instance FromJSON SubredditID where
  parseJSON (String s) =
    SubredditID <$> stripPrefix subredditPrefix s
  parseJSON _ = mempty

instance Thing SubredditID where
  fullName (SubredditID i) = mconcat [subredditPrefix, "_", i]

instance ToQuery SubredditID where
  toQuery k (SubredditID v) = [(k, v)]

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
