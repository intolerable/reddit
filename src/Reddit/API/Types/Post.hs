module Reddit.API.Types.Post where

import Reddit.API.Parser
import Reddit.API.Types.Listing
import Reddit.API.Types.Reddit
import Reddit.API.Types.Thing
import Reddit.API.Types.User

import Control.Applicative
import Data.Aeson
import Data.DateTime as DateTime
import Data.Monoid
import Data.Text (Text)
import Network.API.Builder.Query

newtype PostID = PostID Text
  deriving (Show, Read, Eq, Ord)

instance FromJSON PostID where
  parseJSON j = PostID <$> parseJSON j

instance FromJSON (POSTWrapped PostID) where
  parseJSON (Object o) =
    POSTWrapped <$> ((o .: "json") >>= (.: "data") >>= (.: "id"))
  parseJSON _ = mempty

data Post = Post { postID :: PostID
                 , title :: Text
                 , permalink :: Text
                 , author :: Username
                 , score :: Integer
                 , created :: DateTime
                 , content :: PostContent
                 , liked :: Maybe Bool
                 , flairText :: Maybe Text
                 , domain :: Text
                 , gilded :: Integer
                 , nsfw :: Bool }
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
         <*> (DateTime.fromSeconds <$> d .: "created")
         <*> (buildContent <$> d .: "is_self" <*> d .:? "selftext" <*> d .:? "selftext_html" <*> d .: "url")
         <*> d .:? "likes"
         <*> d .:? "link_flair_text"
         <*> d .: "domain"
         <*> d .: "gilded"
         <*> d .: "over_18"
  parseJSON _ = mempty

data PostContent = SelfPost Text Text
                 | Link Text
                 | TitleOnly
  deriving (Show, Read, Eq)

buildContent :: Bool -> Maybe Text -> Maybe Text -> Maybe Text -> PostContent
buildContent False _ _ (Just url) = Link url
buildContent True (Just s) (Just sHTML) _ = SelfPost s sHTML
buildContent True (Just "") Nothing _ = TitleOnly
buildContent _ _ _ _ = undefined

instance Thing Post where
  fullName p = mconcat [postPrefix , "_", pID]
    where (PostID pID) = postID p

instance Thing PostID where
  fullName (PostID pID) = mconcat [postPrefix , "_", pID]

instance ToQuery PostID where
  toQuery = toQuery . fullName

type PostListing = Listing PostID Post

postPrefix :: Text
postPrefix = "t3"
