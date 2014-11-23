module Reddit.Routes.Subreddit where

import Reddit.Types.Subreddit hiding (title)
import Reddit.Types.SubredditSettings

import Network.API.Builder.Routes

aboutSubreddit :: SubredditName -> Route
aboutSubreddit (R sub) = Route ["r", sub, "about"]
                               []
                               "GET"

subredditSettings :: SubredditName -> Route
subredditSettings (R sub) = Route ["r", sub, "about", "edit"]
                                  []
                                  "GET"

setSubredditSettings :: SubredditID -> SubredditSettings -> Route
setSubredditSettings sr settings =
  Route ["api", "site_admin"]
        [ "sr" =. sr
        , "description" =. sidebarText settings
        , "public_description" =. descriptionText settings
        , "title" =. title settings
        , "link_type" =. linkType settings
        , "comment_score_hide_mins" =. hideScoreMins settings
        , "submit_link_label" =. submitLinkLabel settings
        , "submit_text_label" =. submitTextLabel settings
        , "domain_css" =. domainCSS settings
        , "domain_sidebar" =. domainSidebar settings
        , "show_media" =. showMedia settings
        , "over_18" =. over18 settings
        , "language" =. language settings
        , "wiki_edit_karma" =. wikiEditKarma settings
        , "wiki_edit_age" =. wikiEditAge settings
        , "wikimode" =. wikiEditMode settings
        , "spam_comments" =. spamComments settings
        , "spam_selfposts" =. spamSelfposts settings
        , "spam_links" =. spamLinks settings
        , "public_traffic" =. publicTrafficStats settings
        , "type" =. subredditType settings ]
        "POST"
