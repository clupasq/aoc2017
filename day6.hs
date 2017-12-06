import Test.Hspec
import Debug.Trace

import Data.Map
import Data.Maybe

type Blocks = [Int]

highestIndex :: Blocks -> Int
highestIndex xs = head [ i | i <- [0..length xs - 1], xs!!i == highest]
                           where highest = maximum xs

rotate :: Blocks -> Blocks
rotate xs = last xs : init xs

redistribution :: Int -> Int -> Blocks
redistribution n          1 = [n]
redistribution n blockCount = nextBlock : redistribution (n-nextBlock) (blockCount-1)
  where nextBlock = ceiling (fromIntegral n / fromIntegral blockCount)

reallocate :: Blocks -> Blocks
reallocate xs = zipWith (+) before redist'
  where i           = highestIndex xs
        l           = length xs
        redistStart = (i+1) `mod` l
        before      = take i xs ++ [0] ++ drop (i+1) xs
        avail       = xs !! i
        redist      = redistribution avail l
        redist'     = iterate rotate redist !! redistStart


reallocTrace :: (Blocks, Int, Map Blocks Int) -> (Blocks, Int, Map Blocks Int)
reallocTrace (blocks, index, seen) = (blocks', index+1, seen')
  where blocks' = reallocate blocks
        seen'   = insert blocks index seen

findCycle xs = head $ dropWhile (not . seen) $ steps
  where steps   = iterate reallocTrace (xs, 0, fromList[])
        seen (x, _, m) = x `member` m


cycleLength xs = index
  where (_, index, _) = findCycle xs

loopLength xs = index - indexLastSeen
  where (x, index, m) = findCycle xs
        indexLastSeen = fromJust (Data.Map.lookup x m)

-- cycleLength xs = length $ takeWhile notSeen $ iterate step (xs, fromList [])
--   where step (ys, s)   = let ys' = reallocate ys
--                              s'  = insert ys s
--                          in (ys', s')
--         notSeen (x, s) = x `notMember` s

input = [5, 1, 10, 0, 1, 7, 13, 14, 3, 12, 8, 10, 7, 12, 0, 6]


main = hspec $

  describe "Block allocation" $ do

    it "detects the block to be redistributed" $ do

      highestIndex [0, 0, 0, 0] `shouldBe` 0
      highestIndex [0, 1, 3, 2] `shouldBe` 2
      highestIndex [0, 5, 3, 5] `shouldBe` 1

    it "can rotate a list to the right" $ do

      rotate [1, 2, 3, 4] `shouldBe` [4, 1, 2, 3]
      rotate [0, 2, 0, 0] `shouldBe` [0, 0, 2, 0]
      (iterate rotate [1, 2, 3, 4]) !! 3 `shouldBe` [2, 3, 4, 1]

    it "computes redistribution amount" $ do

      redistribution 7 4 `shouldBe` [2, 2, 2, 1]
      redistribution 2 4 `shouldBe` [1, 1, 0, 0]
      redistribution 100 4 `shouldBe` [25, 25, 25, 25]

    it "reallocates selected block" $ do

      reallocate [1, 2, 3] `shouldBe` [2, 3, 1]
      reallocate [0, 2, 7, 0] `shouldBe` [2, 4, 1, 2]

    it "counts cycle length" $ do

      cycleLength [0, 2, 7, 0] `shouldBe` 5
      print $ cycleLength input

    it "counts loop length" $ do

      loopLength [2, 4, 1, 2] `shouldBe` 4
      print $ loopLength input

