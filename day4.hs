import Data.List
import Test.Hspec

valid :: String -> Bool
valid = unique . words

valid' :: String -> Bool
valid' = unique . map sort . words

unique :: Eq a => [a] -> Bool
unique []     = True
unique (x:xs) = x `notElem` xs && unique xs


main = hspec $

  describe "Passphrase" $ do

    it "invalid if any word repeated" $ do
      valid "aa bb cc dd aa" `shouldBe` False
      valid "aa bb cc cc ee" `shouldBe` False

    it "valid if all words are distinct" $
      valid "aa bb cc dd ee" `shouldBe` True

    it "solves #1" $ do
      inputText <- readFile "input/day4.in"
      let passphrases = lines inputText
      print $ length $ filter valid passphrases

    it "solves #2" $ do
      inputText <- readFile "input/day4.in"
      let passphrases = lines inputText
      print $ length $ filter valid' passphrases

