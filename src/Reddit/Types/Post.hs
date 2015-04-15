module Reddit.Types.Post where

import Reddit.Parser
import Reddit.Types.Listing
import Reddit.Types.Reddit
import Reddit.Types.Subreddit
import Reddit.Types.Thing
import Reddit.Types.User
import Reddit.Utilities

import Control.Applicative
import Data.Aeson
import Data.Time.Clock
import Data.Time.Clock.POSIX
import Data.Monoid
import Data.Text (Text)
import Network.API.Builder.Query
import Prelude

newtype PostID = PostID Text
  deriving (Show, Read, Eq, Ord)

instance FromJSON PostID where
  parseJSON (String s) =
    PostID <$> stripPrefix postPrefix s
  parseJSON _ = mempty

instance FromJSON (POSTWrapped PostID) where
  parseJSON (Object o) =
    POSTWrapped <$> ((o .: "json") >>= (.: "data") >>= (.: "id"))
  parseJSON _ = mempty

data Post = Post { postID :: PostID
                 , title :: Text
                 , permalink :: Text
                 , author :: Username
                 , score :: Integer
                 , created :: UTCTime
                 , content :: PostContent
                 , liked :: Maybe Bool
                 , flairText :: Maybe Text
                 , flairClass :: Maybe Text
                 , domain :: Text
                 , gilded :: Integer
                 , nsfw :: Bool
                 , subredditID :: SubredditID }
  deriving (Show, Read, Eq)

instance FromJSON Post where
  parseJSON (Object o) = do
    o `ensureKind` postPrefix
    d <- o .: "data"
    Post <$> d .: "id"
         <*> d .: "title"
         <*> d .: "permalink"
         <*> d .: "author"
         <*> d .: "score"
         <*> (posixSecondsToUTCTime . fromInteger <$> d .: "created_utc")
         <*> (buildContent <$> d .: "is_self" <*> d .:? "selftext" <*> d .:? "selftext_html" <*> d .: "url")
         <*> d .:? "likes"
         <*> d .:? "link_flair_text"
         <*> d .:? "link_flair_css_class"
         <*> d .: "domain"
         <*> d .: "gilded"
         <*> d .: "over_18"
         <*> d .: "subreddit_id"
  parseJSON _ = mempty

data PostContent = SelfPost Text Text
                 | Link Text
                 | TitleOnly
  deriving (Show, Read, Eq)

buildContent :: Bool -> Maybe Text -> Maybe Text -> Maybe Text -> PostContent
buildContent False _ _ (Just url) = Link url
buildContent True (Just s) (Just sHTML) _ = SelfPost (unescape s) sHTML
buildContent True (Just "") Nothing _ = TitleOnly
buildContent _ _ _ _ = undefined

instance Thing Post where
  fullName p = mconcat [postPrefix , "_", pID]
    where (PostID pID) = postID p

instance Thing PostID where
  fullName (PostID pID) = mconcat [postPrefix , "_", pID]

instance ToQuery PostID where
  toQuery k v = [(k, fullName v)]

type PostListing = Listing PostID Post

postPrefix :: Text
postPrefix = "t3"
