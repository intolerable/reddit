module Reddit.Routes.Flair where

import Reddit.Types.Options
import Reddit.Types.Subreddit
import Reddit.Types.User

import Network.API.Builder.Routes
import Data.Text (Text)
import qualified Data.Text as Text

flairList :: Options UserID -> SubredditName -> Route
flairList opts (R r) =
  Route [ "r", r, "api", "flairlist" ]
        [ "after" =. after opts
        , "before" =. before opts
        , "limit" =. limit opts ]
        "GET"

addLinkFlairTemplate :: SubredditName -> Text -> Text -> Bool -> Route
addLinkFlairTemplate (R sub) css label editable = do
  Route [ "r", sub, "api", "flairtemplate" ]
        [ "css_class" =. css
        , "flair_type" =. ("LINK_FLAIR" :: Text)
        , "text" =. label
        , "text_editable" =. editable ]
        "POST"

flairCSVRoute :: SubredditName -> [(Username, Text, Text)] -> Route
flairCSVRoute (R sub) sets =
  Route [ "r", sub, "api", "flaircsv" ]
        [ "flair_csv" =. Text.unlines (map f sets) ]
        "POST"
  where
    f (Username u, t, c) =
      Text.intercalate "," $ map (Text.pack . show) [u,t,c]

