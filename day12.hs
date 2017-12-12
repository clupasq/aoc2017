import Test.Hspec

import Text.Regex.Posix
import Data.List
import Data.Maybe
import qualified Data.Map as M
import qualified Data.Set as S

type Link = (Int, Int)
type Graph = M.Map Int (S.Set Int)

parse :: String -> [Link]
parse xs = map (\t -> (from, t)) to
  where (from:to) = map read $ getAllTextMatches (xs =~ "[0-9]+")

emptySet :: S.Set Int
emptySet = S.fromList []

addLink :: Link -> Graph -> Graph
addLink (from, to) g = g'
  where lfrom = M.findWithDefault (S.fromList [from]) from g
        lto   = M.findWithDefault (S.fromList [to  ]) to   g
        union = S.union lfrom lto
        elems = S.toList union
        g'    = foldr (\e m -> M.insert e union m) g elems

build :: [Link] -> Graph
build = foldr addLink (M.fromList [])

sampleLinks :: [Link]
sampleLinks = [(0, 2),
               (1, 1),
               (2, 0), (2, 3), (2, 4),
               (3, 2), (3, 4),
               (4, 2), (4, 3), (4, 6),
               (5, 6),
               (6, 4), (6, 5)
              ]

readInput :: IO Graph
readInput = do
  text <- readFile "input/day12.in"
  let links = concatMap parse $ lines text
  return $ build links

main = hspec $ do

  describe "parsing" $ do

    it "can parse a line of input" $ do
      parse "1956 <-> 63, 118" `shouldBe` [(1956, 63), (1956,118)]

    it "can build a graph" $ do
     let graph = build sampleLinks
     M.lookup 0 graph `shouldBe` Just (S.fromList [0, 2, 3, 4, 5, 6])
     M.lookup 1 graph `shouldBe` Just (S.fromList [1])
     M.lookup 2 graph `shouldBe` Just (S.fromList [0, 2, 3, 4, 5, 6])
     M.lookup 3 graph `shouldBe` Just (S.fromList [0, 2, 3, 4, 5, 6])
     M.lookup 4 graph `shouldBe` Just (S.fromList [0, 2, 3, 4, 5, 6])
     M.lookup 5 graph `shouldBe` Just (S.fromList [0, 2, 3, 4, 5, 6])
     M.lookup 6 graph `shouldBe` Just (S.fromList [0, 2, 3, 4, 5, 6])
     M.lookup 7 graph `shouldBe` Nothing


  describe "Questions" $ do

    it "can answer question #1" $ do
      graph <- readInput
      print $ length $ fromJust $ M.lookup 0 graph

    it "can answer question #2" $ do
      graph <- readInput
      let values = map snd $ M.toList graph
      print $ length $ nub values

