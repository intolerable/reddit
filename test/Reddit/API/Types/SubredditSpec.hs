module Reddit.API.Types.SubredditSpec where

import Reddit.API.Types.Subreddit

import Data.ByteString.Lazy (ByteString)
import Data.Either
import Network.API.Builder
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "Reddit.API.Types.Subreddit" $ do
  let decode' = decode :: ByteString -> Either (APIError ()) SubredditID

  it "can parse a SubredditID" $ do
    decode' "\"t5_2s580\"" `shouldBe` Right (SubredditID "2s580")
    decode' "\"2s580\"" `shouldBe` Right (SubredditID "2s580")
    decode' "\"t1_cl1royq\"" `shouldSatisfy` isLeft
