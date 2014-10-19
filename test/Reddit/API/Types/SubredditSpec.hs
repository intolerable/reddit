module Reddit.API.Types.SubredditSpec where

import Data.ByteString.Lazy (ByteString)
import Data.Either
import Network.API.Builder
import Reddit.API.Types.Subreddit
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "Reddit.API.Types.Subreddit" $ do
  let decode' = decode :: ByteString -> Either (APIError ()) SubredditID

  it "should be able to parse a SubredditID" $ do
    decode' "\"t5_2s580\"" `shouldBe` Right (SubredditID "2s580")
    decode' "\"2s580\"" `shouldBe` Right (SubredditID "2s580")
    decode' "\"t1_cl1royq\"" `shouldSatisfy` isLeft
