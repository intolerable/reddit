module Reddit.API.Actions.SubredditSpec where

import Reddit.API.Actions.Subreddit
import Reddit.API.Types.Subreddit

import ConfigLoad
import Data.Either
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "Reddit.API.Actions.Subreddit" $ do
  (reddit, _, subreddit) <- runIO loadConfig

  it "should be able to get the info for a subreddit" $ do
    res <- run reddit $ getSubredditInfo (R "gaming")
    res `shouldSatisfy` isRight

  it "should be able to get the settings for a moderated subreddit" $ do
    res <- run reddit $ getSubredditSettings subreddit
    res `shouldSatisfy` isRight
