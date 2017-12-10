import Test.Hspec

import Data.Char
import Data.Bits
import Numeric (showHex)

reversesub :: [a] -> Int -> Int -> [a]
reversesub xs start size = prefix ++ (reverse mid) ++ suffix
  where prefix = take start xs
        mid    = take size $ drop start xs
        suffix = drop (start + size) xs

reversesubwrap :: [a] -> Int -> Int -> [a]
reversesubwrap xs start size = prefix ++ suffix
  where doubled = xs ++ xs
        processed = reversesub doubled start size
        sufflength = length xs - start
        suffix = take sufflength $ drop start processed
        prefix = take start $ drop (length xs) processed

fst' :: (a, b, c) -> a
fst' (x, _, _) = x

process :: [a] -> [Int] -> [a]
process xs lengths = fst' $ foldl step (xs, 0, 0) lengths
  where step (xs', i, skip) l = (xs'', i', skip + 1)
          where xs'' = reversesubwrap xs' i l
                i'   = (i + l + skip) `mod` (length xs')



keyFromString :: String -> [Int]
keyFromString xs = map ord xs ++ suffix
  where suffix   = [17, 31, 73, 47, 23]

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = take n xs : chunksOf n (drop n xs)


showByteAsHex :: Int -> String
showByteAsHex n | length hex < 2 = '0' : hex
              | otherwise      = hex
  where hex = showHex n ""


sumBlock :: [Int] -> String
sumBlock = showByteAsHex . (foldr1 xor)

hash :: String -> String
hash keyStr = concatMap sumBlock blocks
  where keyBytes = keyFromString keyStr
        finalKey = concat $ replicate 64 keyBytes
        result = process [0..255] finalKey
        blocks = chunksOf 16 result


initial, input :: [Int]
initial = [0..255]
input   = [230,1,2,221,97,252,168,169,57,99,0,254,181,255,235,167]

main = hspec $ do

    describe "Knot Hash" $ do

      it "can reverse substring" $ do
        reversesub [1, 2, 3, 4] 1 2 `shouldBe` [1, 3, 2, 4]

      it "can reverse wrapping substring" $ do
        reversesubwrap [1, 2, 3, 4] 2 3 `shouldBe` [3, 2, 1, 4]

      it "computes final state after reversing given lengths" $ do
        process [0..4] [3, 4, 1, 5] `shouldBe` [3, 4, 2, 1, 0]

      it "can transform key using ASCII codes" $ do
        keyFromString "1,2,3" `shouldBe` [49,44,50,44,51,17,31,73,47,23]

      it "reduces block to a hex repr of xoring its elements" $ do
        sumBlock [65, 27, 9, 1, 4, 3, 40, 50, 91, 7, 6, 0, 2, 5, 68, 22] `shouldBe` "40"

      it "computes hashes correctly" $ do
        hash ""         `shouldBe` "a2582a3a0e66e6e86e3812dcb672a272"
        hash "AoC 2017" `shouldBe` "33efeb34ea91902bb2f59c9920caa6cd"
        hash "1,2,3"    `shouldBe` "3efbe78a8d82f29979031a4aa0b16a9d"
        hash "1,2,4"    `shouldBe` "63960835bcdc130f0b66d7ff4f6a5a8e"


    describe "Questions" $ do

      it "answers question #1" $ do
        let final = process initial input
        print $ product $ take 2 final

      it "answers question #2" $ do
        putStrLn $ hash "230,1,2,221,97,252,168,169,57,99,0,254,181,255,235,167"

