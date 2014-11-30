module Reddit.Actions.SubredditSpec where

import Reddit.Actions.Subreddit
import Reddit.Types.Subreddit

import ConfigLoad
import Data.Either
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "Reddit.Actions.Subreddit" $ do
  (reddit, _, subreddit) <- runIO loadConfig

  it "should be able to get the info for a subreddit" $ do
    let sub = R "gaming"
    res <- run reddit $ getSubredditInfo sub
    res `shouldSatisfy` isRight
    case res of
      Left _ -> expectationFailure "json parse failed"
      Right info -> do
        name info `shouldBe` sub
        subredditID info `shouldBe` SubredditID "2qh03"
        subscribers info `shouldSatisfy` (> 0)

  it "should be able to get the settings for a moderated subreddit" $ do
    res <- run reddit $ getSubredditSettings subreddit
    res `shouldSatisfy` isRight
