module Reddit.Types.Message where

import Reddit.Parser
import Reddit.Types.Comment
import Reddit.Types.Listing
import Reddit.Types.Thing
import Reddit.Types.User

import Control.Applicative
import Data.Aeson
import Data.Maybe
import Data.Monoid
import Data.Text (Text)
import Prelude

data Message = Message { messageID :: MessageKind
                       , new :: Bool
                       , to :: Username
                       , from :: Username
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
            <*> d .: "body"
            <*> d .: "body_html"
            <*> (fromMaybe (Listing Nothing Nothing []) <$> d .:? "replies")
  parseJSON _ = mempty

instance Thing Message where
  fullName m = fullName $ messageID m

data MessageID = MessageID Text
  deriving (Show, Read, Eq)

instance FromJSON MessageID where
  parseJSON (String s) =
    MessageID <$> stripPrefix messagePrefix s
  parseJSON _ = mempty

instance Thing MessageID where
  fullName (MessageID m) = mconcat [messagePrefix, "_", m]

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
