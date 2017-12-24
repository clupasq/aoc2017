import Test.Hspec

import Data.List
import qualified Data.Map as Map
import qualified Data.Set as Set

type Component = (Int, Int)
type Toolbox = Map.Map Int [Component]
type Used      = Set.Set Component

parse :: String -> [Component]
parse = map parseRow . lines
  where parseRow xs = (read a, read b)
          where (a:b:[]) = words xs

add :: Component -> Toolbox -> Toolbox
add c@(x, y) m | x == y    = insertX
               | otherwise = insertBoth
  where insertX    = Map.insertWith (++) x [c] m
        insertBoth = Map.insertWith (++) y [c] insertX

organize :: [Component] -> Toolbox
organize = foldr add Map.empty

value :: Component -> Int
value (x, y) = x + y

strongest :: (Toolbox, Used, Int) -> Int
strongest (avail, used, crtEndpoint) | null possible = 0
                                     | otherwise     = maximum paths
  where comps = Map.findWithDefault [] crtEndpoint avail
        possible = filter (`Set.notMember` used) comps
        paths = map continue possible
        continue c@(x, y) = value c + strongest(avail, used', other)
          where other = if x == crtEndpoint then y else x
                used' = Set.insert c used

longest :: (Toolbox, Used, Int) -> (Int, Int)
longest (avail, used, crtEndpoint) | null possible = (0, 0)
                                   | otherwise     = maximum paths
  where comps = Map.findWithDefault [] crtEndpoint avail
        possible = filter (`Set.notMember` used) comps
        paths = map continue possible
        continue c@(x, y) = (l + 1, s + value c)
          where (l, s) = longest(avail, used', other)
                other = if x == crtEndpoint then y else x
                used' = Set.insert c used


tests = hspec $ describe "Bridge" $ do

  let sample = "0 2\n\
               \2 2\n\
               \2 3\n\
               \3 4\n\
               \3 5\n\
               \0 1\n\
               \10 1\n\
               \9 10"
  let avail = organize (parse sample)

  it "parses the input" $ do
    parse sample `shouldBe` [ (0,2),
                              (2,2),
                              (2,3),
                              (3,4),
                              (3,5),
                              (0,1),
                              (10,1),
                              (9,10)]

  it "computes strongest bridge" $ do
    strongest (avail, Set.empty, 0) `shouldBe` 31

main = do
  tests

  text <- readFile "input/day24.in"
  let avail = organize $ parse text
  putStrLn "Question #1: The strongest bridge:"
  print $ strongest (avail, Set.empty, 0)
  putStrLn "Question #2: The longest bridge of highest possible strength (length, strength):"
  print $ longest (avail, Set.empty, 0)

  print "Done."

