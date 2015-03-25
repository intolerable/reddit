module Reddit.Types.SearchOptions
  ( Order(..) ) where

import Network.API.Builder.Query

data Order = Relevance
           | New
           | Hot
           | Top
           | MostComments
  deriving (Show, Read, Eq)

instance ToQuery Order where
  toQuery k t = return $ (,) k $ case t of
    Relevance -> "relevance"
    New -> "new"
    Hot -> "hot"
    Top -> "top"
    MostComments -> "comments"
