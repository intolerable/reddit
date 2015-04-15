module Reddit.Types.SubredditSettings
  ( SubredditSettings(..) ) where

import Reddit.Parser
import Reddit.Utilities

import Control.Applicative
import Data.Aeson
import Data.Default
import Data.Monoid hiding (Any(..))
import Data.Text (Text)
import Network.API.Builder.Query
import Prelude

data SubredditSettings = SubredditSettings { sidebarText :: Text
                                           , descriptionText :: Text
                                           , title :: Text
                                           , linkType :: ContentOptions
                                           , hideScoreMins :: Integer
                                           , submitLinkLabel :: Maybe Text
                                           , submitTextLabel :: Maybe Text
                                           , domainCSS :: Bool
                                           , domainSidebar :: Bool
                                           , showMedia :: Bool
                                           , over18 :: Bool
                                           , language :: Text
                                           , wikiEditKarma :: Integer
                                           , wikiEditAge :: Integer
                                           , wikiEditMode :: WikiEditMode
                                           , spamComments :: SpamFilterStrength
                                           , spamSelfposts :: SpamFilterStrength
                                           , spamLinks :: SpamFilterStrength
                                           , publicTrafficStats :: Bool
                                           , subredditType :: SubredditType }
  deriving (Show, Read, Eq)

instance FromJSON SubredditSettings where
  parseJSON (Object o) = do
    o `ensureKind` subredditSettingsPrefix
    d <- o .: "data"
    SubredditSettings <$> (unescape <$> d .: "description")
                      <*> d .: "public_description"
                      <*> d .: "title"
                      <*> d .: "content_options"
                      <*> d .: "comment_score_hide_mins"
                      <*> d .: "submit_link_label"
                      <*> d .: "submit_text_label"
                      <*> d .: "domain_css"
                      <*> d .: "domain_sidebar"
                      <*> d .: "show_media"
                      <*> d .: "over_18"
                      <*> d .: "language"
                      <*> d .: "wiki_edit_karma"
                      <*> d .: "wiki_edit_age"
                      <*> d .: "wikimode"
                      <*> d .: "spam_comments"
                      <*> d .: "spam_selfposts"
                      <*> d .: "spam_links"
                      <*> d .: "public_traffic"
                      <*> d .: "subreddit_type"
  parseJSON _ = mempty

instance ToJSON SubredditSettings where
  toJSON settings = object
    [ "description" .= sidebarText settings
    , "public_description" .= descriptionText settings
    , "title" .= title settings
    , "content_options" .= linkType settings
    , "comment_score_hide_mins" .= hideScoreMins settings
    , "submit_link_label" .= submitLinkLabel settings
    , "submit_text_label" .= submitTextLabel settings
    , "domain_css" .= domainCSS settings
    , "domain_sidebar" .= domainSidebar settings
    , "show_media" .= showMedia settings
    , "over_18" .= over18 settings
    , "language" .= language settings
    , "wiki_edit_karma" .= wikiEditKarma settings
    , "wiki_edit_age" .= wikiEditAge settings
    , "wikimode" .= wikiEditMode settings
    , "spam_comments" .= spamComments settings
    , "spam_selfposts" .= spamSelfposts settings
    , "spam_links" .= spamLinks settings
    , "public_traffic" .= publicTrafficStats settings
    , "subreddit_type" .= subredditType settings ]

data SubredditType = Public
                   | Private
                   | Restricted
                   | GoldRestricted
                   | Archived
  deriving (Show, Read, Eq)

subredditTypeText :: SubredditType -> Text
subredditTypeText Public = "public"
subredditTypeText Private = "private"
subredditTypeText Restricted = "restricted"
subredditTypeText GoldRestricted = "gold_restricted"
subredditTypeText Archived = "archived"

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

instance ToJSON SubredditType where
  toJSON = String . subredditTypeText

instance Default SubredditType where
  def = Public

instance ToQuery SubredditType where
  toQuery k v = [(k, subredditTypeText v)]

data ContentOptions = Any
                    | Link
                    | Self
  deriving (Show, Read, Eq)

contentOptionsText :: ContentOptions -> Text
contentOptionsText Any = "any"
contentOptionsText Link = "link"
contentOptionsText Self = "self"

instance FromJSON ContentOptions where
  parseJSON (String s) =
    case s of
      "any" -> return Any
      "link" -> return Link
      "self" -> return Self
      _ -> mempty
  parseJSON _ = mempty

instance ToJSON ContentOptions where
  toJSON = String . contentOptionsText

instance ToQuery ContentOptions where
  toQuery k v = [(k, contentOptionsText v)]

instance Default ContentOptions where
  def = Any

data SpamFilterStrength = FilterLow
                        | FilterHigh
                        | FilterAll
  deriving (Show, Read, Eq)

instance FromJSON SpamFilterStrength where
  parseJSON (String s) =
    case s of
      "low" -> return FilterLow
      "high" -> return FilterHigh
      "all" -> return FilterAll
      _ -> mempty
  parseJSON _ = mempty

spamFilterStrengthText :: SpamFilterStrength -> Text
spamFilterStrengthText FilterLow = "low"
spamFilterStrengthText FilterHigh = "high"
spamFilterStrengthText FilterAll = "all"

instance ToJSON SpamFilterStrength where
  toJSON = String . spamFilterStrengthText

instance ToQuery SpamFilterStrength where
  toQuery k v = [(k, spamFilterStrengthText v)]

data WikiEditMode = Anyone
                  | ApprovedOnly
                  | ModOnly
  deriving (Show, Read, Eq)

instance FromJSON WikiEditMode where
  parseJSON (String s) =
    case s of
      "disabled" -> return ModOnly
      "modonly" -> return ApprovedOnly
      "anyone" -> return Anyone
      _ -> mempty
  parseJSON _ = mempty

wikiEditModeText :: WikiEditMode -> Text
wikiEditModeText ModOnly = "disabled"
wikiEditModeText Anyone = "anyone"
wikiEditModeText ApprovedOnly = "modonly"

instance ToJSON WikiEditMode where
  toJSON = String . wikiEditModeText

instance ToQuery WikiEditMode where
  toQuery k v = [(k, wikiEditModeText v)]

instance Default WikiEditMode where
  def = Anyone

subredditSettingsPrefix :: Text
subredditSettingsPrefix = "subreddit_settings"
