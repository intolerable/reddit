module Reddit.API.Types.SubredditSettings where

import Reddit.API.Parser

import Control.Applicative
import Data.Default
import Data.Aeson
import Data.Monoid (mempty)
import Data.Text (Text)


data SubredditSettings = SubredditSettings { sidebarText :: Text
                                           , descriptionText :: Text
                                           , linkType :: ContentOptions
                                           , hideScoreMins :: Integer
                                           , submitLinkLabel :: Maybe Text
                                           , submitTextLabel :: Maybe Text
                                           , domainCSS :: Bool
                                           , domainSidebar :: Bool
                                           , subredditType :: SubredditType }
  deriving (Show, Read, Eq)

instance FromJSON SubredditSettings where
  parseJSON (Object o) = do
    o `ensureKind` subredditSettingsPrefix
    d <- o .: "data"
    SubredditSettings <$> d .: "description"
                      <*> d .: "public_description"
                      <*> d .: "content_options"
                      <*> d .: "comment_score_hide_mins"
                      <*> d .: "submit_link_label"
                      <*> d .: "submit_text_label"
                      <*> d .: "domain_css"
                      <*> d .: "domain_sidebar"
                      <*> d .: "subreddit_type"
  parseJSON _ = mempty

data SubredditType = Public
                   | Private
                   | Restricted
                   | GoldRestricted
                   | Archived
  deriving (Show, Read, Eq)

instance FromJSON SubredditType where
  parseJSON (String s) =
    case s of
      "public" -> return Public
      "private" -> return Private
      "restricted" -> return Restricted
      "gold_restricted" -> return GoldRestricted
      "archived" -> return Archived
      _ -> mempty
  parseJSON _ = mempty

instance Default SubredditType where
  def = Public

data ContentOptions = Any
                    | Link
                    | Self
  deriving (Show, Read, Eq)

instance FromJSON ContentOptions where
  parseJSON (String s) = 
    case s of
      "any" -> return Any
      "link" -> return Link
      "self" -> return Self
      _ -> mempty
  parseJSON _ = mempty

instance Default ContentOptions where
  def = Any

subredditSettingsPrefix :: Text
subredditSettingsPrefix = "subreddit_settings"