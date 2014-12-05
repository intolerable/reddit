module Reddit.Types.PostSpec where

import Reddit.Types.Post

import Data.ByteString.Lazy (ByteString)
import Data.Either
import Network.API.Builder
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "Reddit.Types.Post" $ do
  let decode' = eitherDecode :: ByteString -> Either String PostID

  it "can parse a PostID" $ do
    decode' "\"t3_1n1qrg\"" `shouldBe` Right (PostID "1n1qrg")
    decode' "\"1n1qrg\"" `shouldBe` Right (PostID "1n1qrg")
    decode' "\"t5_2s580\"" `shouldSatisfy` isLeft
