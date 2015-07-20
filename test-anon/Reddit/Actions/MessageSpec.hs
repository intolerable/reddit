module Reddit.Actions.MessageSpec where

import Reddit

import Data.Either
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "Reddit.Actions.Message" $ do

  it "shouldn't have an inbox for an anonymous user" $ do
    res <- runRedditAnon getInbox
    res `shouldSatisfy` isLeft
