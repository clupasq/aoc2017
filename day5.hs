import Test.Hspec

import qualified Data.Sequence as S

-- initial (slow) implementation with lists
type ListWithIndex = ([Int], Int)

step1 :: ListWithIndex -> ListWithIndex
step1 (xs, i) = (xs', i')
  where prefix  = take i xs
        suffix  = drop (i+1) xs
        current = xs !! i
        i'      = i + current
        xs'     = prefix ++ [current+1] ++ suffix

step2 :: ListWithIndex -> ListWithIndex
step2 (xs, i) = (xs', i')
  where prefix    = take i xs
        suffix    = drop (i+1) xs
        current   = xs !! i
        i'        = i + current
        newOffset = if current > 2
                       then current - 1
                       else current + 1
        xs'       = prefix ++ [newOffset] ++ suffix

countSteps :: (ListWithIndex -> ListWithIndex) -> ListWithIndex -> Int
countSteps stepFn = length . takeWhile insideBounds . iterate stepFn
  where insideBounds (xs', i') = i' >= 0 && i' < length xs'


-- faster, using Data.Sequence

type SeqWithIndex = (S.Seq Int, Int)

step1' :: SeqWithIndex -> SeqWithIndex
step1' (xs, i) = (xs', i')
  where i'  = i + xs `S.index` i
        xs' = S.adjust (+1) i xs

step2' :: SeqWithIndex -> SeqWithIndex
step2' (xs, i) = (xs', i')
  where i'    = i + xs `S.index` i
        upd n | n > 2     = pred n
              | otherwise = succ n
        xs'   = S.adjust upd i xs

countSteps' :: (SeqWithIndex -> SeqWithIndex) -> SeqWithIndex -> Int
countSteps' stepFn = length . takeWhile insideBounds . iterate stepFn
  where insideBounds (xs', i') = i' >= 0 && i' < length xs'

main = hspec $

  describe "List jumping" $ do


    it "can make an iteration" $ do

       step1' (S.fromList [0, 3, 0, 1, -3], 0) `shouldBe` (S.fromList [1, 3, 0, 1, -3], 0)
       step1' (S.fromList [1, 3, 0, 1, -3], 0) `shouldBe` (S.fromList [2, 3, 0, 1, -3], 1)
       step1' (S.fromList [2, 3, 0, 1, -3], 1) `shouldBe` (S.fromList [2, 4, 0, 1, -3], 4)
       step1' (S.fromList [2, 4, 0, 1, -3], 4) `shouldBe` (S.fromList [2, 4, 0, 1, -2], 1)
       step1' (S.fromList [2, 4, 0, 1, -2], 1) `shouldBe` (S.fromList [2, 5, 0, 1, -2], 5)

    it "can count number of steps" $ do

       countSteps' step1' (S.fromList [0, 3, 0, 1, -3], 0) `shouldBe` 5
       countSteps' step2' (S.fromList [0, 3, 0, 1, -3], 0) `shouldBe` 10


    it "solves #1" $ do

      inputText <- readFile "input/day5.in"
      let xs = S.fromList $ map read $ lines inputText
      print $ countSteps' step1' (xs, 0)

    it "solves #2" $ do
      -- still slow... ~257s

      inputText <- readFile "input/day5.in"
      let xs = S.fromList $ map read $ lines inputText
      print $ countSteps' step2' (xs, 0)

