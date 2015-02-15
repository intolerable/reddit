module Reddit.Types.SearchOptions where

import Network.API.Builder.Query

data Order = Relevance
           | New
           | Hot
           | Top
           | MostComments
  deriving (Show, Read, Eq)

instance ToQuery Order where
  toQuery t = Just $ case t of
    Relevance -> "relevance"
    New -> "new"
    Hot -> "hot"
    Top -> "top"
    MostComments -> "comments"
