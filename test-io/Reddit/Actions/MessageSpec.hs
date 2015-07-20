module Reddit.Actions.MessageSpec where

import Reddit.Actions.Message

import ConfigLoad
import Test.Hspec

isRight :: Either a b -> Bool
isRight = const False `either` const True

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "Reddit.Actions.Message" $ do
  (reddit, _, _) <- runIO loadConfig

  it "should be able to check inbox" $ do
    res <- run reddit $ getInbox
    res `shouldSatisfy` isRight

  it "should be able to check unread messages" $ do
    res <- run reddit $ getUnread
    res `shouldSatisfy` isRight
