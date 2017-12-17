import Test.Hspec


rotate :: Int -> [a] -> [a]
rotate n xs | n < 0     = rotate (l-n) xs
            | otherwise = take l $ drop n' $ cycle xs
  where l  = length xs
        n' = n `mod` l

spinlockStep :: Int -> [Int] -> Int -> [Int]
spinlockStep rot xs n = n: rotate (rot+1) xs

tests = hspec $ do

  describe "Spinlock" $ do

    it "can rotate the buffer" $ do
      rotate 3 [0] `shouldBe` [0]
      rotate 3 "abcdefghij" `shouldBe` "defghijabc"

    it "can rotate the buffer in reverse" $ do
      rotate (-1) [0, 1, 2] `shouldBe` [1, 2, 0]


    it "can iterate correctly" $ do
      spinlockStep 3 [0] 1 `shouldBe` [1, 0]
      spinlockStep 3 [1, 0] 2 `shouldBe` [2, 1, 0]
      spinlockStep 3 [2, 1, 0] 3 `shouldBe` [3, 1, 0, 2]
      spinlockStep 3 [8, 6, 1, 0, 5, 7, 2, 4, 3] 9 `shouldBe`
        [9, 5, 7, 2, 4, 3, 8, 6, 1, 0]


question1 :: IO ()
question1 = do
  let spinlock = spinlockStep 354
  let finalResult = foldl spinlock [0] [1..2017]
  putStr "Answer #1: "
  print $ finalResult !! 1

main = do
  tests
  question1
  -- this will be too slow. Solving in Ruby...
  -- question2

