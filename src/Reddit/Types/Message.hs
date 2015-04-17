module Reddit.Types.Message where

import Reddit.Parser
import Reddit.Types.Comment
import Reddit.Types.Listing
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

data Message = Message { messageID :: MessageKind
                       , new :: Bool
                       , to :: Username
                       , from :: Username
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
            <*> d .: "author"
            <*> d .: "subject"
            <*> (unescape <$> d .: "body")
            <*> (unescape <$> d .: "body_html")
            <*> (fromMaybe (Listing Nothing Nothing []) <$> d .:? "replies")
  parseJSON _ = mempty

instance Thing Message where
  fullName m = fullName $ messageID m

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
  deriving (Show, Read, Eq)

instance FromJSON MessageID where
  parseJSON (String s) =
    MessageID <$> stripPrefix messagePrefix s
  parseJSON _ = mempty

instance Thing MessageID where
  fullName (MessageID m) = mconcat [messagePrefix, "_", m]

instance ToQuery MessageID where
  toQuery k m = toQuery k (fullName m)

messagePrefix :: Text
messagePrefix = "t4"

data MessageKind = CommentMessage CommentID
                 | PrivateMessage MessageID
  deriving (Show, Read, Eq)

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
