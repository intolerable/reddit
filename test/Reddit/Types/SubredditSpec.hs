module Reddit.Types.SubredditSpec where

import Reddit.Types.Subreddit

import Data.Aeson
import Data.ByteString.Lazy (ByteString)
import Test.Hspec

isLeft :: Either a b -> Bool
isLeft = const True `either` const False

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "Reddit.Types.Subreddit" $ do
  let decode' = eitherDecode :: ByteString -> Either String SubredditID

  it "can compare subreddits" $ do
    R "intolerable_test" `shouldBe` R "intolerable_test"
    R "intolerable_test" `shouldBe` R "Intolerable_test"
    R "intolerable_test" `shouldBe` R "INTOLERABLE_TEST"

  it "can parse a SubredditID" $ do
    decode' "\"t5_2s580\"" `shouldBe` Right (SubredditID "2s580")
    decode' "\"2s580\"" `shouldBe` Right (SubredditID "2s580")
    decode' "\"t1_cl1royq\"" `shouldSatisfy` isLeft
