module Reddit.Actions.FlairSpec where

import Reddit
import Reddit.Types.Subreddit

import Data.Either
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "Reddit.Actions.Flair" $ do

  it "shouldn't be able to get the flair list anonymously" $ do
    res <- runRedditAnon $ getFlairList $ R "gaming"
    res `shouldSatisfy` isLeft
