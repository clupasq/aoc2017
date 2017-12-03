import Test.Hspec
import qualified Data.Map as M

input = 265149

data Direction = North | South | East | West
type Coords = (Int, Int)

toLeft :: Direction -> Direction
toLeft North = West
toLeft West  = South
toLeft South = East
toLeft East  = North

advance :: Direction -> Coords -> Coords
advance North (x, y) = (x, y-1)
advance West  (x, y) = (x-1, y)
advance South (x, y) = (x, y+1)
advance East  (x, y) = (x+1, y)

duplicateEach :: [a] -> [a]
duplicateEach (x:xs) = x : x : duplicateEach xs
duplicateEach     [] = []

buildEdge :: (Coords, Direction) -> Int -> [Coords]
buildEdge (start, d) len = tail $ scanl (const . advance d) start [1..len]

buildSpiral :: Coords -> Direction -> [Coords]
buildSpiral origin direction = (concat $ map fst $ scanl step ([origin], direction) lengths)
  where lengths = duplicateEach [1..]
        step (formerCoords, dir) edgeLen = (buildEdge (start, newDir) edgeLen, newDir)
          where start  = last formerCoords
                newDir = toLeft dir

spiral = buildSpiral (0, 0) South

distance :: Int -> Int
distance n = abs x + abs y
  where (x, y) = spiral !! (n-1)


neighbours :: Coords -> [Coords]
neighbours (x, y) = [(x-1, y-1),
                     (x-1, y  ),
                     (x-1, y+1),
                     (x  , y-1),
                     (x  , y+1),
                     (x+1, y-1),
                     (x+1, y  ),
                     (x+1, y+1)]

sumNeighbours :: M.Map Coords Int -> Coords -> Int
sumNeighbours valueMap coords = sum $ map findVal $ neighbours coords
  where findVal c = M.findWithDefault 0 c valueMap

spiralValues :: [Int]
spiralValues = tail $ map fst $ scanl step (0, M.fromList []) spiral
  where step (_, dict) coord = (value, newDict)
          where value = if coord == (0, 0)
                           then 1
                           else sumNeighbours dict coord
                newDict = M.insert coord value dict

answer2 = head $ dropWhile (<input) spiralValues


main = hspec $ do

  describe "spiral" $ do

    it "can build edges" $ do

      buildEdge (( 0,  0), East ) 1 `shouldBe` [( 1,  0)]
      buildEdge (( 1,  0), North) 1 `shouldBe` [( 1, -1)]
      buildEdge (( 1, -1), West ) 2 `shouldBe` [( 0, -1), (-1, -1)]
      buildEdge ((-1, -1), South) 2 `shouldBe` [(-1,  0), (-1,  1)]

    it "can build infinitely" $ do

      -- print $ map (spiral!!) [0..10]
      spiral !! 0 `shouldBe` (0, 0)
      spiral !! 1 `shouldBe` (1, 0)
      spiral !! 9 `shouldBe` (2, 1)


    it "can measure Manhattan distance" $ do

      -- print $ distance input
      distance 1 `shouldBe` 0
      distance 2 `shouldBe` 1
      distance 12 `shouldBe` 3
      distance 23 `shouldBe` 2
      distance 1024 `shouldBe` 31

    it "can compute values as sums of values of neighbours" $ do

      print answer2
      take 10 spiralValues `shouldBe` [1, 1, 2, 4, 5, 10, 11, 23, 25, 26]


