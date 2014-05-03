module Reddit.API.Types.Message where

import Reddit.API.Types.Comment
import Reddit.API.Types.Listing
import Reddit.API.Types.Thing
import Reddit.API.Types.User

import Control.Applicative
import Data.Aeson
import Data.Monoid
import Data.Text (Text)

data Message = Message { messageID :: MessageKind
                       , new :: Bool
                       , to :: Username
                       , from :: Username 
                       , body :: Text
                       , bodyHTML :: Text 
                       , replies :: Listing Message }
  deriving (Show, Read, Eq)

instance FromJSON Message where
  parseJSON (Object o) = do
    d <- o .: "data"
    Message <$> parseJSON (Object o)
            <*> d .: "new"
            <*> d .: "dest"
            <*> d .: "author"
            <*> d .: "body"
            <*> d .: "body_html"
            <*> (maybe (Listing []) id <$> d .:? "replies")
  parseJSON _ = mempty

instance Thing Message where
  fullName m = fullName $ messageID m

data MessageID = MessageID Text
  deriving (Show, Read, Eq)

instance FromJSON MessageID where
  parseJSON (String s) = return $ MessageID s
  parseJSON _ = mempty

instance Thing MessageID where
  fullName (MessageID m) = mconcat ["t4_", m]

data MessageKind = CommentMessage CommentID
                 | PrivateMessage MessageID
  deriving (Show, Read, Eq)

instance FromJSON MessageKind where
  parseJSON (Object o) = do
    kind <- o .: "kind"
    d <- o .: "data"
    case kind of 
      String "t1" -> CommentMessage <$> d .: "id"
      String "t4" -> PrivateMessage <$> d .: "id"
      String _ -> error "Unrecognized message type"
      _ -> mempty
  parseJSON _ = mempty

instance Thing MessageKind where
  fullName (CommentMessage c) = fullName c
  fullName (PrivateMessage p) = fullName p