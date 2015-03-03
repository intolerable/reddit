module Reddit.Types.User where

import Reddit.Parser
import Reddit.Types.Thing

import Control.Applicative
import Data.Aeson
import Data.DateTime
import Data.Monoid (mconcat, mempty)
import Data.Text (Text)
import Network.API.Builder.Query

newtype Username = Username Text
  deriving (Show, Read, Eq)

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
                 , userCreated :: DateTime
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
         <*> (fromSeconds <$> d .: "created_utc")
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

userPrefix :: Text
userPrefix = "t2"
