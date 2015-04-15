module Reddit.Types.Moderation where

import Reddit.Parser
import Reddit.Types.User
import Reddit.Types.Thing

import Control.Applicative
import Data.Aeson
import Data.Time.Clock
import Data.Time.Clock.POSIX
import Data.Monoid
import Data.Text (Text)
import Network.API.Builder.Query
import Prelude

newtype BanID = BanID Text
  deriving (Show, Read, Eq, Ord)

instance FromJSON BanID where
  parseJSON (String s) =
    BanID <$> stripPrefix banPrefix s
  parseJSON _ = mempty

instance ToQuery BanID where
  toQuery k v = [(k, fullName v)]

instance Thing BanID where
  fullName (BanID b) = mconcat [banPrefix, "_", b]

data Ban = Ban { username :: Username
               , userID :: UserID
               , note :: Text
               , since :: UTCTime }
  deriving (Show, Read, Eq)

instance FromJSON Ban where
  parseJSON (Object o) =
    Ban <$> o .: "name"
        <*> o .: "id"
        <*> o .: "note"
        <*> (posixSecondsToUTCTime . fromInteger <$> o .: "date")
  parseJSON _ = mempty

banPrefix :: Text
banPrefix = "rb"
