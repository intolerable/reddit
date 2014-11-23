module Reddit.Types.Wiki where

import Reddit.Parser
import Reddit.Types.User

import Control.Applicative
import Data.Aeson
import Data.DateTime (DateTime)
import Data.Monoid (mempty)
import Data.Text (Text)
import qualified Data.DateTime as DateTime
import qualified Data.Text as Text

newtype RevisionID = RevisionID Text
  deriving (Show, Read, Eq)

data WikiPage = WikiPage { contentHTML :: Text
                         , contentMarkdown :: Text
                         , revisionDate :: DateTime
                         , revisedBy :: Username
                         , canRevise :: Bool }
  deriving (Show, Read, Eq)

unescape :: Text -> Text
unescape = replace "&gt;" ">" . replace "&lt;" "<" . replace "&amp;" "&"
  where replace s r = Text.intercalate r . Text.splitOn s

instance FromJSON WikiPage where
  parseJSON (Object o) = do
    o `ensureKind` "wikipage"
    d <- o .: "data"
    WikiPage <$> (unescape <$> d .: "content_html")
             <*> (unescape <$> d .: "content_md")
             <*> (DateTime.fromSeconds <$> d .: "revision_date")
             <*> ((d .: "revision_by") >>= (.: "data") >>= (.: "name"))
             <*> d .: "may_revise"
  parseJSON _ = mempty
