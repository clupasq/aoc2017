import Test.Hspec

import qualified Data.Map as M
import Data.Maybe

data Program = Program { name :: String,
                          weight :: Int,
                          children :: [String]
                        } deriving (Show, Eq)

parseIntInParens :: String -> Int
parseIntInParens xs = read $ (init.tail) xs

trim :: String -> String
trim xs | last xs == ',' = init xs
        | otherwise      = xs

parseChildList :: [String] -> [String]
parseChildList        [] = []
parseChildList ("->":xs) = parseChildList xs
parseChildList (   x:xs) = trim x : parseChildList xs

parse :: String -> Program
parse xs = Program name weight children
  where (name:weight':children') = words xs
        weight   = parseIntInParens weight'
        children = parseChildList children'

getAllPrograms :: IO [Program]
getAllPrograms = do
  input <- readFile "input/day7.in"
  let programs = map parse $ lines input
  return programs

root :: [Program] -> Program
root ps = head $ filter (\p -> name p `notElem` allChildren) ps
  where allChildren = concatMap children ps

getChildren :: M.Map String Program -> Program -> [Program]
getChildren m p = [fromJust $ M.lookup c m | c <- children p]

getTotalWeight :: M.Map String Program -> Program -> Int
getTotalWeight m p = weight p + childrenTotalWeights
  where childrenTotalWeights = sum [getTotalWeight m c | c <- getChildren m p]

unique :: Eq a => [a] -> Bool
unique []     = True
unique (x:xs) = all (==x) xs

getChildrenWeights :: M.Map String Program -> Program -> [(String, Int)]
getChildrenWeights m p = [(name c, getTotalWeight m c) | c <- getChildren m p]


main = hspec $ do

  describe "Recursive programs" $ do

    describe "Parsing" $ do

      it "line without children" $ do

        parse "abcdef (10)" `shouldBe` Program { name = "abcdef",
                                                 weight = 10,
                                                 children = []}
        parse "test (20)"   `shouldBe` Program { name = "test",
                                                 weight = 20,
                                                 children = []}

      it "line with children" $ do

        parse "nsqfu (1724) -> tgvzdov, ksuhtas, arbrp, nbpkmt"
          `shouldBe` Program { name = "nsqfu",
                               weight = 1724,
                               children = (words "tgvzdov ksuhtas arbrp nbpkmt")}
        parse "njvsnz (128) -> lztme, txkog, yypfse"
          `shouldBe` Program { name = "njvsnz",
                               weight = 128,
                               children = (words "lztme txkog yypfse")}

    it "solves #1" $ do
      programs <- getAllPrograms
      putStrLn "The root is:"
      print $ root programs

    it "solves #2" $ do
      programs <- getAllPrograms
      let weights = M.fromList $ map (\p -> (name p, p)) programs
      let programChildWeights = map (\p -> (name p, getChildrenWeights weights p)) programs

      let unbalanced = filter (\(_, c) -> (not.unique) (map snd c)) programChildWeights
      -- print unbalanced
      let toFix = fromJust $ M.lookup "drjmjug" weights
      print $ weight toFix - 8

      pending






