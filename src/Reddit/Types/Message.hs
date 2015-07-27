module Reddit.Types.Message where

import Reddit.Parser
import Reddit.Types.Comment
import Reddit.Types.Listing
import Reddit.Types.Reddit
import Reddit.Types.Thing
import Reddit.Types.User
import Reddit.Utilities

import Control.Applicative
import Data.Aeson
import Data.Maybe
import Data.Monoid
import Data.Text (Text)
import Network.API.Builder.Query
import Prelude
import qualified Data.Vector as Vector

data Message = Message { messageID :: MessageKind
                       , new :: Bool
                       , to :: Username
                       , from :: Maybe Username
                       , subject :: Text
                       , body :: Text
                       , bodyHTML :: Text
                       , replies :: Listing MessageKind Message }
  deriving (Show, Read, Eq)

instance FromJSON Message where
  parseJSON (Object o) = do
    d <- o .: "data"
    Message <$> d .: "name"
            <*> d .: "new"
            <*> d .: "dest"
            <*> d .:? "author"
            <*> (d .: "link_title" <|> d .: "subject")
            <*> (unescape <$> d .: "body")
            <*> (unescape <$> d .: "body_html")
            <*> (fromMaybe (Listing Nothing Nothing []) <$> d .:? "replies")
  parseJSON _ = mempty

instance Thing Message where
  fullName m = fullName $ messageID m

instance ToQuery Message where
  toQuery k v = [(k, fullName v)]

isPrivateMessage :: Message -> Bool
isPrivateMessage m =
  case messageID m of
    PrivateMessage _ -> True
    _ -> False

isCommentReply :: Message -> Bool
isCommentReply m =
  case messageID m of
    CommentMessage _ -> True
    _ -> False

data MessageID = MessageID Text
  deriving (Show, Read, Eq, Ord)

instance FromJSON MessageID where
  parseJSON (String s) =
    MessageID <$> stripPrefix messagePrefix s
  parseJSON _ = mempty

instance Thing MessageID where
  fullName (MessageID m) = mconcat [messagePrefix, "_", m]

instance ToQuery MessageID where
  toQuery k m = toQuery k (fullName m)

instance FromJSON (POSTWrapped MessageID) where
  parseJSON (Object o) = do
    ms <- (o .: "json") >>= (.: "data") >>= (.: "things")
    case Vector.toList ms of
      [v] -> POSTWrapped <$> (v .: "data" >>= (.: "id"))
      _ -> mempty
  parseJSON _ = mempty

messagePrefix :: Text
messagePrefix = "t4"

data MessageKind = CommentMessage CommentID
                 | PrivateMessage MessageID
  deriving (Show, Read, Eq, Ord)

instance FromJSON MessageKind where
  parseJSON s =
    (CommentMessage <$> parseJSON s) <|>
    (PrivateMessage <$> parseJSON s)

instance Thing MessageKind where
  fullName (CommentMessage c) = fullName c
  fullName (PrivateMessage p) = fullName p

instance ToQuery MessageKind where
  toQuery k (CommentMessage c) = toQuery k c
  toQuery k (PrivateMessage p) = toQuery k p
