import Test.Hspec

import Data.Bits


allBitsSet :: Int -> Int
-- allBitsSet b = 2^b - 1
allBitsSet = pred . (2^)

set16bits, set31bits :: Int
set16bits = allBitsSet 16
set31bits = allBitsSet 31

generate :: Int -> Int -> [Int]
generate f n = tail $ iterate step n
  where step x = x * f `mod` set31bits

generateA = generate 16807
generateB = generate 48271

match :: Int -> Int -> Bool
match x y = lsb16 x == lsb16 y
  where lsb16 = (set16bits .&.)

countMatches :: Int -> [Int] -> [Int] -> Int
countMatches len ga gb = length $ filter (uncurry match) $ take len (zip ga gb)

divisibleBy :: Int -> Int -> Bool
divisibleBy n = (==0) . (`mod` n)

tests = hspec $ do

  describe "Generators" $ do

    it "can compute the sequence of values" $ do
      take 5 (generateA 65) `shouldBe` [ 1092455,
                                         1181022009,
                                         245556042,
                                         1744312007,
                                         1352636452]
      take 5 (generateB 8921) `shouldBe` [ 430625591,
                                           1233683848,
                                           1431495498,
                                           137874439,
                                           285222916]

    it "can detect matches" $ do
      match    1092455  430625591 `shouldBe` False
      match 1181022009 1233683848 `shouldBe` False
      match  245556042 1431495498 `shouldBe` True
      match 1744312007  137874439 `shouldBe` False
      match 1352636452  285222916 `shouldBe` False

    it "can count matches between generators" $ do
      let ga = generateA 65
      let gb = generateB 8921
      countMatches 5 ga gb `shouldBe` 1

    -- takes ~133s
    it "can count matches between generators over 40M pairs" $ do
      let ga = generateA 65
      let gb = generateB 8921
      countMatches 40000000 ga gb `shouldBe` 588

main = do
  tests
  let ga = generateA 699
  let gb = generateB 124
  putStrLn "Question #1:"
  print $ countMatches 40000000 ga gb
  let ga' = filter (divisibleBy 4) ga
  let gb' = filter (divisibleBy 8) gb
  putStrLn "Question #2:"
  print $ countMatches 5000000 ga' gb'
  putStrLn "Done."


