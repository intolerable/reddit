module Reddit.API.Types.Comment where

import Reddit.API.Parser
import Reddit.API.Types.Listing
import Reddit.API.Types.Post hiding (author)
import Reddit.API.Types.Reddit
import Reddit.API.Types.Thing
import Reddit.API.Types.User

import Control.Applicative
import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.DateTime as DateTime
import Data.Monoid
import Data.Text (Text)
import Data.Vector ((!?))
import Network.API.Builder.Query
import qualified Data.Text as Text
import qualified Data.Vector as V

newtype CommentID = CommentID Text
  deriving (Show, Read, Eq, Ord)

instance FromJSON CommentID where
  parseJSON (String s) = return $ CommentID s
  parseJSON _ = mempty

instance Thing CommentID where
  fullName (CommentID cID) = Text.concat [commentPrefix, "_", cID]

instance ToQuery CommentID where
  toQuery = toQuery . fullName

instance FromJSON (POSTWrapped CommentID) where
  parseJSON (Object o) = do
    ts <- (o .: "json") >>= (.: "data") >>= (.: "things")
    case ts !? 0 of
      Just v -> POSTWrapped <$> (v .: "data" >>= (.: "id"))
      Nothing -> mempty
  parseJSON _ = mempty

data CommentReference = Reference Integer [CommentID]
                      | Actual Comment
  deriving (Show, Read, Eq)

instance FromJSON CommentReference where
  parseJSON a@(Object o) = do
    k <- o .: "kind"
    case k of
      String "t1" -> Actual <$> parseJSON a
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

isActual :: CommentReference -> Bool
isActual (Actual _) = True
isActual _ = False

isReference :: CommentReference -> Bool
isReference (Reference _ _) = True
isReference _ = False

data Comment = Comment { commentID :: CommentID
                       , score :: Maybe Integer
                       , gilded :: Integer
                       , saved :: Bool
                       , author :: Username
                       , authorFlairCSSClass :: Maybe Text
                       , authorFlairText :: Maybe Text
                       , body :: Text
                       , bodyHTML :: Text
                       , replies :: Listing CommentID CommentReference
                       , created :: DateTime
                       , edited :: Maybe DateTime
                       , parentLink :: PostID
                       , inReplyTo :: Maybe CommentID }
  deriving (Show, Read, Eq)

instance FromJSON Comment where
  parseJSON (Object o) = do
    o `ensureKind` commentPrefix
    d <- o .: "data"
    Comment <$> d .: "id"
            <*> d .:? "score"
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
            <*> (parsePostID =<< d .: "link_id")
            <*> ((>>= parseCommentID) <$> d .:? "parent_id")
    where getDate (Number i) =
            Just $ DateTime.fromSeconds $ round i
          getDate _ = Nothing
          parsePostID s =
            maybe mempty (return . PostID) $ Text.stripPrefix (postPrefix <> "_") s
          parseCommentID s =
            CommentID <$> Text.stripPrefix (commentPrefix <> "_") s
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
    case V.toList a of
      postListing:commentListing:_ -> do
        Listing _ _ (post:[]) <- parseJSON postListing :: Parser (Listing PostID Post)
        Listing _ _ comments <- parseJSON commentListing :: Parser (Listing CommentID CommentReference)
        return $ PostComments post comments
      _ -> mempty
  parseJSON _ = mempty

type CommentListing = Listing CommentID Comment

commentPrefix :: Text
commentPrefix = "t1"
