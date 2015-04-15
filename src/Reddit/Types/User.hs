module Reddit.Types.User where

import Reddit.Parser
import Reddit.Types.Thing

import Control.Applicative
import Data.Aeson
import Data.Monoid (mconcat, mempty)
import Data.Text (Text)
import Data.Time.Clock
import Data.Time.Clock.POSIX
import Network.API.Builder.Query
import Prelude hiding (mconcat, mempty)
import qualified Data.Text as Text
import qualified Data.Vector as Vector

newtype Username = Username Text
  deriving (Show, Read)

instance Eq Username where
  Username x == Username y = Text.toLower x == Text.toLower y

instance FromJSON Username where
  parseJSON (String s) = return $ Username s
  parseJSON _ = mempty

instance ToQuery Username where
  toQuery k (Username user) = [(k, user)]

newtype UserID = UserID Text
  deriving (Show, Read, Eq)

instance FromJSON UserID where
  parseJSON (String s) =
    UserID <$> stripPrefix userPrefix s
  parseJSON _ = mempty

instance Thing UserID where
  fullName (UserID u) = mconcat [userPrefix, "_", u]

instance ToQuery UserID where
  toQuery k v = [(k, fullName v)]

data User = User { userID :: Text
                 , userName :: Username
                 , userCreated :: UTCTime
                 , linkKarma :: Integer
                 , commentKarma :: Integer
                 , hasMail :: Maybe Bool
                 , hasModMail :: Maybe Bool
                 , isFriend :: Bool
                 , userIsOver18 :: Maybe Bool
                 , isMod :: Bool
                 , hasGold :: Bool
                 , hasVerifiedEmail :: Bool } deriving (Show, Eq)

instance FromJSON User where
  parseJSON (Object o) = do
    o `ensureKind` userPrefix
    d <- o .: "data"
    User <$> d .: "id"
         <*> d .: "name"
         <*> (posixSecondsToUTCTime . fromInteger <$> d .: "created_utc")
         <*> d .: "link_karma"
         <*> d .: "comment_karma"
         <*> d .:? "has_mail"
         <*> d .:? "has_mod_mail"
         <*> d .: "is_friend"
         <*> d .:? "over_18"
         <*> d .: "is_mod"
         <*> d .: "is_gold"
         <*> d .: "has_verified_email"
  parseJSON _ = mempty

newtype UserList = UserList [Relationship]
  deriving (Show, Read, Eq)

instance FromJSON UserList where
  parseJSON (Object o) = do
    o `ensureKind` "UserList"
    UserList <$> ((o .: "data") >>= (.: "children"))
  parseJSON (Array a) = do
    case Vector.toList a of
      [o] -> parseJSON o
      [o, _] -> parseJSON o
      _ -> mempty
  parseJSON _ = fail "wat"

data Relationship =
  Relationship { relationUsername :: Username
               , relationUserID :: UserID
               , relationSince :: UTCTime
               , relationNote :: Maybe Text }
  deriving (Show, Read, Eq)

instance FromJSON Relationship where
  parseJSON (Object o) =
    Relationship <$> o .: "name"
                 <*> o .: "id"
                 <*> (posixSecondsToUTCTime . fromInteger <$> o .: "date")
                 <*> (f <$> o .:? "note")
    where f (Just "") = Nothing
          f x = x
  parseJSON _ = fail "Relationship should be an object"

userPrefix :: Text
userPrefix = "t2"
