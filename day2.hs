import Data.Maybe
import Debug.Trace

minMaxDiff :: String -> Int
minMaxDiff xs = maxi - mini
  where numbers = map read $ words xs
        mini    = minimum numbers
        maxi    = maximum numbers


evenDiv :: Int -> Int -> Maybe Int
evenDiv x y | x `mod` y == 0 = Just $ x `div` y
            | otherwise      = Nothing

lineEvenDiv :: String -> Int
lineEvenDiv s = fromJust $ head [ed | x <- ns,
                                      y <- ns,
                                      x /= y,
                                      ed <- [x `evenDiv` y],
                                      isJust ed]
              where ns = map read (words s)

checksumUsing :: (String -> Int) -> String -> Int
checksumUsing lineFunc = sum . (\x -> trace (show x) x) . map lineFunc . lines

main = do
  input <- readFile "input/day2.in"
  -- part 1
  print $ checksumUsing minMaxDiff input
  -- part 2
  print $ checksumUsing lineEvenDiv input

