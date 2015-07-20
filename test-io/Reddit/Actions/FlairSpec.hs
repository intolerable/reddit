module Reddit.Actions.FlairSpec where

import Reddit.Actions.Flair

import ConfigLoad
import Data.Either
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "Reddit.Actions.Flair" $ do
  (reddit, anon, _, subreddit) <- runIO loadConfig

  it "should be able to get the flair list" $ do
    res <- run reddit $ getFlairList subreddit
    res `shouldSatisfy` isRight

  it "shouldn't be able to get the flair list anonymously" $ do
    res <- run anon $ getFlairList subreddit
    res `shouldSatisfy` isLeft
