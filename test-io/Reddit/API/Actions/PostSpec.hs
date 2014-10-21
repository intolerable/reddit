module Reddit.API.Actions.PostSpec where

import Reddit.API.Actions.Post
import Reddit.API.Types.Listing
import Reddit.API.Types.Post
import Reddit.API.Types.Subreddit (SubredditID(..))
import Reddit.API.Types.User

import ConfigLoad
import Data.Either
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "Reddit.API.Actions.Post" $ do
  (reddit, _, _) <- runIO loadConfig

  it "should be able to get info for a post" $ do
    res <- run reddit $ getPostInfo (PostID "z1c9z")
    res `shouldSatisfy` isRight
    case res of
      Left _ -> expectationFailure "something failed"
      Right post -> do
        author post `shouldBe` Username "PresidentObama"
        title post `shouldBe` "I am Barack Obama, President of the United States -- AMA"
        subredditID post `shouldBe` SubredditID "2qzb6"
        nsfw post `shouldBe` False

  it "should be able to get info for multiple posts" $ do
    res <- run reddit $ getPostsInfo [PostID "z1c9z", PostID "t0ynr"]
    res `shouldSatisfy` isRight

  it "should cope with getting info for no posts" $ do
    res <- run reddit $ getPostsInfo []
    res `shouldSatisfy` isRight
    case res of
      Left _ -> expectationFailure "something failed"
      Right (Listing _ _ ps) ->
        ps `shouldBe` []

  it "should be able to get mass PostIDs" $ do
    let a = take 100 postIDs
    res <- run reddit $ getPostsInfo a
    res `shouldSatisfy` isRight
    case res of
      Left _ -> expectationFailure "something failed"
      Right (Listing _ _ ps) -> do
        length ps `shouldBe` length a

  it "should fail if it tries to get TOO many PostIDs" $ do
    let a = take 101 postIDs
    res <- run reddit $ getPostsInfo a
    res `shouldSatisfy` isLeft

postIDs :: [PostID]
postIDs = [PostID "2jt0sd",PostID "2jsysb",PostID "2jssi5",PostID "2jsqm4",PostID "2jrr8b",PostID "2jr648",PostID "2jqjyw",PostID "2jpe9u",PostID "2joqoy",PostID "2jn54v",PostID "2jm3bn",PostID "2jltfr",PostID "2jlbi9",PostID "2jk3ez",PostID "2jec9n",PostID "2jb8fa",PostID "2ja5kg",PostID "2j7t1n",PostID "2j5i39",PostID "2j4xky",PostID "2j4va7",PostID "2j3v32",PostID "2j26ab",PostID "2j18o0",PostID "2izady",PostID "2iw357",PostID "2ivw4y",PostID "2iu6ik",PostID "2ityv5",PostID "2ismwa",PostID "2iq81l",PostID "2ip4ry",PostID "2iou19",PostID "2iniv7",PostID "2inrkn",PostID "2inlie",PostID "2inbdn",PostID "2inby7",PostID "2in8rn",PostID "2in57t",PostID "2inboq",PostID "2in766",PostID "2in6sl",PostID "2imdly",PostID "2ilzmz",PostID "2ii254",PostID "2iehx9",PostID "2iaa3s",PostID "2iaddi",PostID "2i06yi",PostID "2hz9l9",PostID "2hvifo",PostID "2hvezx",PostID "2huvc9",PostID "2hsrfs",PostID "2hpp5j",PostID "2hnhsq",PostID "2hmyo1",PostID "2his00",PostID "2hifan",PostID "2hhitw",PostID "2hdwkq",PostID "2hdt6i",PostID "2hcpi6",PostID "2hbpmg",PostID "2h90uv",PostID "2h4nf0",PostID "2h43lz",PostID "2h3rlx",PostID "2h1pqw",PostID "2h02ly",PostID "2gz6h6",PostID "2gyvt9",PostID "2gxvjw",PostID "2gvicb",PostID "2gusy9",PostID "2gtpc8",PostID "2gti8v",PostID "2gt1k1",PostID "2gt2vl",PostID "2gsz1s",PostID "2grp5b",PostID "2grnka",PostID "24j9o6",PostID "2gr9pg",PostID "2got1d",PostID "2gnvze",PostID "2glpx2",PostID "2ghsx1",PostID "2ghbku",PostID "2gge9x",PostID "2gd141",PostID "2g75uy",PostID "2g42c9",PostID "2fxci0",PostID "2fum17",PostID "2fq9n0",PostID "2fo1gd",PostID "2fny7i",PostID "2flkol",PostID "2fkydl"]
