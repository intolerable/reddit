module Reddit.API.Types.Comment where

import Reddit.API.Parser
import Reddit.API.Types.Listing
import Reddit.API.Types.Post
import Reddit.API.Types.Reddit
import Reddit.API.Types.Thing
import Reddit.API.Types.User

import APIBuilder.Query
import Control.Applicative
import Data.Aeson
import Data.DateTime as DateTime
import Data.Monoid
import Data.Text (Text)
import Data.Vector ((!?))
import qualified Data.Text as T
import qualified Data.Vector as V

newtype CommentID = CommentID T.Text
  deriving (Show, Read, Eq)

instance FromJSON CommentID where
  parseJSON (String s) = return $ CommentID s
  parseJSON _ = mempty

instance FromJSON (POSTWrapped CommentID) where
  parseJSON (Object o) = do
    ts <- (o .: "json") >>= (.: "data") >>= (.: "things")
    case ts !? 0 of
      Just v -> POSTWrapped <$> (v .: "data" >>= (.: "id"))
      Nothing -> mempty
  parseJSON _ = mempty

data Comment = Comment { commentID :: CommentID
                       , ups :: Maybe Integer
                       , downs :: Maybe Integer
                       , gilded :: Integer
                       , saved :: Bool
                       , author :: Username
                       , authorFlairCSSClass :: Maybe Text
                       , authorFlairText :: Maybe Text
                       , body :: Text
                       , bodyHTML :: Text
                       , replies :: Listing Comment
                       , created :: DateTime
                       , edited :: Maybe DateTime
                       , scoreHidden :: Bool }
  deriving (Show, Read, Eq)

instance FromJSON Comment where
  parseJSON (Object o) = do
    o `ensureKind` commentPrefix
    d <- o .: "data"
    Comment <$> d .: "id"
            <*> d .:? "ups"
            <*> d .:? "downs"
            <*> d .: "gilded"
            <*> d .: "saved"
            <*> d .: "author"
            <*> d .:? "author_flair_css_class"
            <*> d .:? "author_flair_text"
            <*> d .: "body"
            <*> d .: "body_html"
            <*> d .: "replies"
            <*> (DateTime.fromSeconds <$> d .: "created")
            <*> (getDate <$> d .: "edited")
            <*> d .: "score_hidden"
    where getDate (Number i) = Just $ DateTime.fromSeconds $ round i
          getDate _ = Nothing
  parseJSON _ = mempty

data PostComments = PostComments Post [Comment]
  deriving (Show, Read, Eq)

instance FromJSON PostComments where
  parseJSON (Array a) =
    case V.toList a of
      postListing:commentListing:_ -> do
        Listing (post:[]) <- parseJSON postListing
        Listing comments <- parseJSON commentListing
        return $ PostComments post comments
      _ -> mempty
  parseJSON _ = mempty

instance Thing CommentID where
  fullName (CommentID cID) = T.concat [commentPrefix, "_", cID]

instance ToQuery CommentID where
  toQuery = toQuery . fullName

commentPrefix :: Text
commentPrefix = "t1"
