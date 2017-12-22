import Test.Hspec

import Data.List
import Data.Maybe
import qualified Data.Map as M

type Row      = String
type Grid     = [Row]
type RuleBook = M.Map Grid Grid



split :: String -> String -> [String]
split _  [] = [[]]
split ss t@(x:xs) | ss `isPrefixOf` t = "" : split ss (drop (length ss) t)
                  | otherwise         = (x:r) : rs
                  where (r:rs) = split ss xs

chunksOf :: Int -> [a] -> [[a]]
chunksOf n [] = []
chunksOf n xs = pref : chunksOf n suff
  where (pref, suff) = splitAt n xs


parseRule :: String -> (Grid, Grid)
parseRule ss = (mkGrid from, mkGrid to)
  where (from:to:[]) = split " => " ss
        mkGrid = split "/"

parseRules :: String -> RuleBook
parseRules text = foldr addRule M.empty $ lines text
  where addRule l m = let (from, to) = parseRule l
                          in M.insert from to m

getRules :: IO RuleBook
getRules = do
  text <- readFile "input/day21.in"
  return $ parseRules text

splitInGridsOf :: Int -> Grid -> [Grid]
splitInGridsOf n g = concatMap splitRowChunk rowChunks
  where size = length g
        chunkCount = size `div` n
        rowChunks = chunksOf n g
        splitRowChunk rs = [ map ((!!(i-1)) . chunksOf n) rs | i <- [1..chunkCount]]


splitGrid :: Grid -> [Grid]
splitGrid g | even $ length g = splitInGridsOf 2 g
            | otherwise       = splitInGridsOf 3 g

intSqrt :: Int -> Int
intSqrt = fromIntegral . floor . sqrt . fromIntegral

combineGrids :: [Grid] -> Grid
combineGrids gs     = concatMap combineRow rowChunks
  where gridsPerRow = intSqrt $ length gs
        rowChunks   = chunksOf gridsPerRow gs
        combineRow  = foldr1 (zipWith (++))

transformations :: Grid -> [Grid]
transformations g = rotations ++ (map flip rotations)
  where rotateCW  = (transpose . reverse)
        rotations = take 4 $ iterate rotateCW g
        flip      = map reverse

growPiece :: RuleBook -> Grid -> Grid
growPiece m g = fromJust $ head $ filter isJust $ map (\k -> M.lookup k m)  (transformations g)

grow :: RuleBook -> Grid -> Grid
grow m g = combineGrids [ growPiece m p | p <- splitGrid g]

initialGrid = [".#.",
               "..#",
               "###"]

tests = hspec $ describe "Fractal art" $ do
  describe "Parsing" $ do

    it "can split string" $ do
      split " => " "###/###/.#. => #.##/##.#/#.#./#..."
        `shouldBe` ["###/###/.#.", "#.##/##.#/#.#./#..."]
      split "/" "###/###/.#." `shouldBe` ["###", "###", ".#."]

    it "can make chunks" $ do
      chunksOf 3 "abcdef" `shouldBe` ["abc", "def"]
      chunksOf 2 "abcdef" `shouldBe` ["ab", "cd", "ef"]

  describe "Transformations" $ do

    it "can split a 4x4 grid" $ do
      splitGrid ["abcd",
                 "....",
                 "xyzw",
                 "####"] `shouldBe` [["ab",
                                      ".."],
                                     ["cd",
                                      ".."],
                                     ["xy",
                                      "##"],
                                     ["zw",
                                      "##"]]

    it "can paste a bunch of grids into one" $ do
      combineGrids [["ab",
                      ".."],
                    ["cd",
                      ".."],
                    ["xy",
                      "##"],
                    ["zw",
                      "##"]] `shouldBe` ["abcd",
                                        "....",
                                        "xyzw",
                                        "####"]

    it "can generate all possible rotations/flips" $ do
      let ts = transformations ["ab",
                                "cd"]
      ts `shouldContain` [["ab",
                           "cd"],
                          ["ca",
                           "db"],
                          ["dc",
                           "ba"],
                          ["bd",
                           "ac"],
                          ["ba",
                           "dc"],
                          ["ac",
                           "bd"],
                          ["cd",
                           "ab"],
                          ["db",
                           "ca"]]

    it "can grow" $ do
      let rules = parseRules "../.# => ##./#../...\n\
                             \.#./..#/### => #..#/..../..../#..#"

      grow rules initialGrid `shouldBe` ["#..#",
                                         "....",
                                         "....",
                                         "#..#"]
      let g = grow rules
      (g.g) initialGrid `shouldBe` [ "##.##.",
                                     "#..#..",
                                     "......",
                                     "##.##.",
                                     "#..#..",
                                     "......"]




main = do
  tests
  rules <- getRules
  let countSetBits = length . filter (=='#') . concat

  let after5 = head $ drop 5 $ iterate (grow rules) initialGrid
  putStrLn "Question #1:"
  print $ countSetBits after5

  mapM print $ map countSetBits  $ iterate (grow rules) initialGrid
  -- prints the 18th in ~30 mins: 3081737
  print "Done."

