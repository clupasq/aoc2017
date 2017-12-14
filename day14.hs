import Test.Hspec
import Debug.Trace

import Data.Char (ord)
import Data.Maybe (fromJust)
import Data.Bits (xor)
import Data.List (nub, isPrefixOf)
import Text.Printf (printf)
import qualified Data.Map as M

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

hash :: String -> [Int]
hash keyStr = map sumBlock blocks
  where keyBytes = keyFromString keyStr
        finalKey = concat $ replicate 64 keyBytes
        result = process [0..255] finalKey
        blocks = chunksOf 16 result
        sumBlock = foldr1 xor

binaryHash :: String -> String
binaryHash keyStr = concatMap (\i -> printf "%08b" i) (hash keyStr)


count :: (a -> Bool) -> [a] -> Int
count _    []     = 0
count pred (x:xs) = count pred xs + if pred x then 1 else 0


type Coord = (Int, Int)

buildBlockMap :: [String] -> M.Map Coord Coord
buildBlockMap rows = M.fromList [(b, b) | b <- usedBlocks ]
  where range = [0..length rows - 1]
        usedBlocks = [(y, x) | y <- range, x <- range, rows !! y !! x ==  '1']


neighbours :: Coord -> [Coord]
neighbours (y, x) = [(y-1, x  ),
                     (y+1, x  ),
                     (y  , x+1),
                     (y  , x-1)]

linkNeighbours :: Coord -> M.Map Coord Coord -> M.Map Coord Coord
linkNeighbours c@(y, x) m = m'
  where ns = [n | n <- neighbours c, M.member n m]
        getRegion z = fromJust $ M.lookup z m
        regions  = getRegion c : map getRegion ns
        regionId = minimum regions
        m' = foldr (\n mm -> M.insert n regionId mm) m ns

detectRegions :: M.Map Coord Coord -> Int
detectRegions m = length $ nub $ M.elems $ trace (show blocksAssigned) blocksAssigned
  where blocks         = M.keys m
        blocksAssigned = foldl (flip linkNeighbours) m blocks






-- countRegions :: [String] -> Int
-- countRegions rows 


testDisk = ["11001",
            "10011",
            "00100",
            "11101",
            "10001"]

tests = hspec $ do
  describe "Defrag" $ do

    it "can compute knot hashes" $ do
      binaryHash "flqrgnkx-0" `shouldSatisfy` ("11010100" `isPrefixOf`)
      binaryHash "flqrgnkx-1" `shouldSatisfy` ("01010101" `isPrefixOf`)
      binaryHash "flqrgnkx-2" `shouldSatisfy` ("00001010" `isPrefixOf`)
      binaryHash "flqrgnkx-3" `shouldSatisfy` ("10101101" `isPrefixOf`)
      binaryHash "flqrgnkx-4" `shouldSatisfy` ("01101000" `isPrefixOf`)
      binaryHash "flqrgnkx-5" `shouldSatisfy` ("11001001" `isPrefixOf`)
      binaryHash "flqrgnkx-6" `shouldSatisfy` ("01000100" `isPrefixOf`)
      binaryHash "flqrgnkx-7" `shouldSatisfy` ("11010110" `isPrefixOf`)

    it "can build a block map" $ do
      buildBlockMap testDisk `shouldBe` M.fromList [((0,0),(0,0)),((0,1),(0,1)),((0,4),(0,4)),((1,0),(1,0)),((1,3),(1,3)),((1,4),(1,4)),((2,2),(2,2)),((3,0),(3,0)),((3,1),(3,1)),((3,2),(3,2)),((3,4),(3,4)),((4,0),(4,0)),((4,4),(4,4))]

    it "can count continuous regions" $ do
      pending




input :: String
input   = "stpzcrnm"

keys :: [String]
keys = map (\i -> input ++ '-' : show i) [0..127]

file = "input/day14.out"

writeHashes :: IO ()
writeHashes = do
  let rows = map binaryHash keys
  writeFile file (unlines rows)
  -- let usedCount = count (=='1') $ concat rows
  -- print usedCount

readHashes :: IO [String]
readHashes = do
  text <- readFile file
  return $ lines text


main = do
  tests
  -- writeHashes
  -- h <- readHashes
  -- print $ buildBlockMap h
  -- print h



