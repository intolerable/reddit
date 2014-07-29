module Reddit.API.Types.Wiki where

import Reddit.API.Parser
import Reddit.API.Types.User

import Control.Applicative
import Data.Aeson
import Data.DateTime (DateTime)
import Data.Monoid (mempty)
import Data.Text (Text)
import qualified Data.DateTime as DateTime


newtype RevisionID = RevisionID Text
  deriving (Show, Read, Eq)

data WikiPage = WikiPage { contentHTML :: Text
                         , contentMarkdown :: Text
                         , revisionDate :: DateTime
                         , revisedBy :: Username
                         , canRevise :: Bool }
  deriving (Show, Read, Eq)

instance FromJSON WikiPage where
  parseJSON (Object o) = do
    o `ensureKind` "wikipage"
    d <- o .: "data"
    WikiPage <$> d .: "content_html"
             <*> d .: "content_md"
             <*> (DateTime.fromSeconds <$> d .: "revision_date")
             <*> ((d .: "revision_by") >>= (.: "data") >>= (.: "name"))
             <*> d .: "may_revise"
  parseJSON _ = mempty
