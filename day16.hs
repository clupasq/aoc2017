import Test.Hspec

import qualified Data.Map as M
import Data.List (sortOn, groupBy)

type Formation = M.Map Char Int
data Move = Spin Int | Exchange Int Int | Partner Char Char
          deriving (Eq, Show)

mkFormation :: String -> Formation
mkFormation xs = M.fromList $ zip xs [0..]

toString :: Formation -> String
toString = map fst . sortOn snd . M.toList

parsePair :: String -> (String, String)
parsePair xs = (sa, sb)
  where [sa, ssb]= groupBy (\a b -> b /= '/') xs
        sb = tail ssb

parse :: String -> Move
parse ('s':xs) = Spin $ read xs
parse ('x':xs) = Exchange (read f) (read t) where (f, t) = parsePair xs
parse ('p':xs) = Partner  (head f) (head t) where (f, t) = parsePair xs

apply :: Formation -> Move -> Formation
apply f (Spin x) = M.fromList $ map slideIndex $ M.toList f
  where l = length f
        slideIndex (a, i) = (a, (i+x) `mod` l)
apply f (Exchange x y) = apply f (Partner c1 c2)
  where pairs = M.toList f
        c1    = head [ c | (c, i) <- pairs, i == x]
        c2    = head [ c | (c, i) <- pairs, i == y]
apply f (Partner c1 c2) = f'
  where i1 = (M.!) f c1
        i2 = (M.!) f c2
        f' = M.insert c1 i2 (M.insert c2 i1 f)

dance :: String -> [Move] -> String
dance s = toString . foldl apply (mkFormation s)

repeatedDance :: String -> Int -> [Move] -> String
repeatedDance s count ms = toString $ foldr step (mkFormation s) [1..count]
  where step _ f = foldl apply f ms

tests = hspec $ do
  describe "Dance" $ do

    it "builds formation from string" $ do
      mkFormation "abcde" `shouldBe` M.fromList [ ('a', 0),
                                                  ('b', 1),
                                                  ('c', 2),
                                                  ('d', 3),
                                                  ('e', 4)]

    it "can write formation back to string" $ do
      toString (M.fromList [ ('a', 0),
                             ('b', 1),
                             ('c', 2),
                             ('d', 3),
                             ('e', 4)]) `shouldBe` "abcde"

    describe "Moves" $ do

      it "Spin" $ do
        dance "abcdef" [Spin 2] `shouldBe` "efabcd"

      it "Exchange" $ do
        dance "abcdef" [Exchange 1 3] `shouldBe` "adcbef"

      it "Partner" $ do
        dance "abcdef" [Partner 'c' 'e'] `shouldBe` "abedcf"

  describe "Input parsing" $ do

    it "parses Spin commands" $ do
      parse "s12" `shouldBe` Spin 12

    it "parses Exchange commands" $ do
      parse "x15/8" `shouldBe` Exchange 15 8

    it "parses Partner commands" $ do
      parse "pa/c" `shouldBe` Partner 'a' 'c'


readInputMoves :: IO [Move]
readInputMoves = do
  text <- readFile "input/day16.in"
  return $ map parse $ lines text

main = do
  tests
  moves <- readInputMoves
  putStrLn "Question #1:"
  putStrLn $ dance ['a'..'p'] moves
  putStrLn "Question #2:"
  putStrLn $ repeatedDance ['a'..'p'] 1000 moves



