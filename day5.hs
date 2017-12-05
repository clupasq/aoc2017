import Test.Hspec

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
countSteps step1fn = length . takeWhile insideBounds . iterate step1fn
  where insideBounds (xs', i') = i' >= 0 && i' < length xs'

main = hspec $

  describe "List jumping" $ do

    it "can make an iteration" $ do

       step1 ([0, 3, 0, 1, -3], 0) `shouldBe` ([1, 3, 0, 1, -3], 0)
       step1 ([1, 3, 0, 1, -3], 0) `shouldBe` ([2, 3, 0, 1, -3], 1)
       step1 ([2, 3, 0, 1, -3], 1) `shouldBe` ([2, 4, 0, 1, -3], 4)
       step1 ([2, 4, 0, 1, -3], 4) `shouldBe` ([2, 4, 0, 1, -2], 1)
       step1 ([2, 4, 0, 1, -2], 1) `shouldBe` ([2, 5, 0, 1, -2], 5)

    it "can count number of steps" $ do

       countSteps step1 ([0, 3, 0, 1, -3], 0) `shouldBe` 5
       countSteps step2 ([0, 3, 0, 1, -3], 0) `shouldBe` 10


    it "solves #1" $ do

      inputText <- readFile "input/day5.in"
      let xs = map read $ lines inputText
      print $ countSteps step1 (xs, 0)

    it "solves #2" $ do

      pending
      -- todo: make this faster if possible
      -- print $ countSteps step2 (xs, 0)

