module Reddit.Types.Comment where

import Reddit.Parser
import Reddit.Types.Listing
import Reddit.Types.Post hiding (author)
import Reddit.Types.Reddit
import Reddit.Types.Subreddit
import Reddit.Types.Thing
import Reddit.Types.User
import Reddit.Utilities

import Control.Applicative
import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.Time.Clock
import Data.Time.Clock.POSIX
import Data.Monoid
import Data.Text (Text)
import Data.Traversable
import Network.API.Builder.Query
import Prelude
import qualified Data.Text as Text
import qualified Data.Vector as Vector

newtype CommentID = CommentID Text
  deriving (Show, Read, Eq, Ord)

instance FromJSON CommentID where
  parseJSON (String s) =
    CommentID <$> stripPrefix commentPrefix s
  parseJSON _ = mempty

instance Thing CommentID where
  fullName (CommentID cID) = Text.concat [commentPrefix, "_", cID]

instance ToQuery CommentID where
  toQuery k v = [(k, fullName v)]

instance FromJSON (POSTWrapped CommentID) where
  parseJSON (Object o) = do
    ts <- (o .: "json") >>= (.: "data") >>= (.: "things")
    case Vector.toList ts of
      [v] -> POSTWrapped <$> (v .: "data" >>= (.: "id"))
      _ -> mempty
  parseJSON _ = mempty

data CommentReference = Reference Integer [CommentID]
                      | Actual Comment
  deriving (Show, Read, Eq)

instance FromJSON CommentReference where
  parseJSON v@(Object o) = do
    k <- o .: "kind"
    case k of
      String "t1" -> Actual <$> parseJSON v
      String "more" ->
        Reference <$> ((o .: "data") >>= (.: "count"))
                  <*> ((o .: "data") >>= (.: "children"))
      _ -> mempty
  parseJSON _ = mempty

instance FromJSON (POSTWrapped [CommentReference]) where
  parseJSON (Object o) = do
    cs <- (o .: "json") >>= (.: "data") >>= (.: "things")
    POSTWrapped <$> parseJSON cs
  parseJSON _ = mempty

-- | @isReference c@ returns is true if @c@ is an actual comment, false otherwise
isActual :: CommentReference -> Bool
isActual (Actual _) = True
isActual _ = False

-- | @isReference c@ returns is true if @c@ is a reference, false otherwise
isReference :: CommentReference -> Bool
isReference (Reference _ _) = True
isReference _ = False

data Comment = Comment { commentID :: CommentID
                       , score :: Maybe Integer
                       , subredditID :: SubredditID
                       , subreddit :: SubredditName
                       , gilded :: Integer
                       , saved :: Bool
                       , author :: Username
                       , authorFlairCSSClass :: Maybe Text
                       , authorFlairText :: Maybe Text
                       , body :: Text
                       , bodyHTML :: Text
                       , replies :: Listing CommentID CommentReference
                       , created :: UTCTime
                       , edited :: Maybe UTCTime
                       , parentLink :: PostID
                       , inReplyTo :: Maybe CommentID }
  deriving (Show, Read, Eq)

instance FromJSON Comment where
  parseJSON (Object o) = do
    o `ensureKind` commentPrefix
    d <- o .: "data"
    Comment <$> d .: "id"
            <*> d .:? "score"
            <*> d .: "subreddit_id"
            <*> d .: "subreddit"
            <*> d .: "gilded"
            <*> d .: "saved"
            <*> d .: "author"
            <*> d .:? "author_flair_css_class"
            <*> d .:? "author_flair_text"
            <*> (unescape <$> d .: "body")
            <*> d .: "body_html"
            <*> d .: "replies"
            <*> (posixSecondsToUTCTime . fromInteger <$> d .: "created_utc")
            <*> ((fmap (posixSecondsToUTCTime . fromInteger) <$> d .: "edited") <|> (pure Nothing))
            <*> (parseJSON =<< d .: "link_id")
            <*> (d .:? "parent_id" >>= \v -> traverse parseJSON v <|> pure Nothing)
  parseJSON _ = mempty

instance FromJSON (POSTWrapped Comment) where
  parseJSON (Object o) = do
    ts <- (o .: "json") >>= (.: "data") >>= (.: "things")
    case Vector.toList ts of
      [c] -> POSTWrapped <$> parseJSON c
      _ -> mempty
  parseJSON _ = mempty

flattenComments :: CommentReference -> [CommentReference]
flattenComments a@(Actual c) = a : concatMap flattenComments ((\(Listing _ _ cs) -> cs) $ replies c)
flattenComments (Reference _ rs) = map (\r -> Reference 1 [r]) rs

isDeleted :: Comment -> Bool
isDeleted = (== Username "[deleted]") . author

data PostComments = PostComments Post [CommentReference]
  deriving (Show, Read, Eq)

instance FromJSON PostComments where
  parseJSON (Array a) =
    case Vector.toList a of
      postListing:commentListing:_ -> do
        Listing _ _ [post] <- parseJSON postListing :: Parser (Listing PostID Post)
        Listing _ _ comments <- parseJSON commentListing :: Parser (Listing CommentID CommentReference)
        return $ PostComments post comments
      _ -> mempty
  parseJSON _ = mempty

type CommentListing = Listing CommentID Comment

commentPrefix :: Text
commentPrefix = "t1"
