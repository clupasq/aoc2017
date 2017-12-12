import Test.Hspec

import Text.Regex.Posix
import qualified Data.Map as M
import qualified Data.Set as S

type Link = (Int, [Int])
type Graph = M.Map Int (S.Set Int)

parse :: String -> Link
parse xs = (from, to)
  where (from:to) = map read $ getAllTextMatches (xs =~ "[0-9]+")

emptySet :: S.Set Int
emptySet = S.fromList []

add :: Link -> Graph -> Graph
add (from, to) m = foldr makeLink m to
  where makeLink t m = M.insert from s' m
          where s  = M.findWithDefault emptySet from m
                s' = S.insert t s

build :: [Link] -> Graph
build ls = foldr add (M.fromList []) ls

reachableFrom :: Graph -> Int -> S.Set Int -> S.Set Int
reachableFrom m i visited = S.union neighbours (S.unions reachables)
  where neighbours = M.findWithDefault emptySet i m
        notVisited = S.toList $ (S.\\) neighbours visited
        visited'   = S.union visited neighbours
        reachables = map (\n -> reachableFrom m n visited') notVisited



sampleLinks :: [Link]
sampleLinks = [(0, [2]),
               (1, [1]),
               (2, [0, 3, 4]),
               (3, [2, 4]),
               (4, [2, 3, 6]),
               (5, [6]),
               (6, [4, 5])
              ]

main = hspec $ do

  describe "parsing" $ do

    it "can parse a line of input" $ do
      parse "1956 <-> 63, 118" `shouldBe` (1956, [63, 118])

  describe "graph building" $ do

    it "builds the graph as a Map" $ do
      let graph = build sampleLinks
      reachableFrom graph 0 emptySet `shouldBe` S.fromList [0, 2, 3, 4, 5, 6]


  describe "Questions" $ do

    it "answers question #1" $ do
      input <- readFile "input/day12.in"
      let links = map parse $ lines input
      let graph = build links
      let answer = reachableFrom graph 0 emptySet
      print $ length (S.toList answer)



