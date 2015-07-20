module Reddit.Actions.MessageSpec where

import Reddit.Actions.Message

import ConfigLoad
import Data.Either
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "Reddit.Actions.Message" $ do
  (reddit, anon, _, _) <- runIO loadConfig

  it "should be able to check inbox" $ do
    res <- run reddit $ getInbox
    res `shouldSatisfy` isRight

  it "should be able to check unread messages" $ do
    res <- run reddit $ getUnread
    res `shouldSatisfy` isRight

  it "shouldn't have an inbox for an anonymous user" $ do
    res <- run anon getInbox
    res `shouldSatisfy` isLeft
