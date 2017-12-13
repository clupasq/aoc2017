import Test.Hspec
import Debug.Trace

type Layer = (Int, Int)
type Time = Int

cycleLength :: Int -> Int
cycleLength range = range * 2 - 2

caught :: Time -> Layer -> Bool
caught t l@(i, range) = t `mod` cycleLength range == 0

severity :: Layer -> Int
severity (idx, range) = idx * range

totalSeverity :: [Layer] -> Int
totalSeverity ls = sum $ map severity $ filter (\l@(i,r) -> caught i l) $ ls

escapeUncaught :: Int -> [Layer] -> Bool
escapeUncaught initDelay ls = not $ any (\l@(i,r) -> caught (initDelay+i) l) ls

safeWait :: [Layer] -> Int
safeWait ls = head $ [i | i <- [0..], escapeUncaught i ls]

tests = hspec $ do
  describe "Firewall" $ do

    it "knows when it gets caught" $ do
      caught 0 (0, 3) `shouldBe` True
      caught 6 (6, 4) `shouldBe` True
      caught 4 (4, 4) `shouldBe` False

    it "computes layer severity when caught" $ do
      severity (6, 4) `shouldBe` 6*4
      severity (4, 3) `shouldBe` 4*3

    it "computes total severity correctly" $ do
      totalSeverity testInput `shouldBe` 24

    it "computes wait time to avoid being caught" $ do
      safeWait testInput `shouldBe` 10

main = do
  tests
  let answer1 = totalSeverity input
  putStrLn $ "Total severity:     " ++ (show answer1)
  let answer2 = safeWait input
  putStrLn $ "Min safe wait time: " ++ (show answer2)


testInput :: [Layer]
testInput = [(0, 3),
             (1, 2),
             (4, 4),
             (6, 4)]

input :: [Layer]
input = [ (0, 3),
          (1, 2),
          (2, 4),
          (4, 8),
          (6, 5),
          (8, 6),
          (10, 6),
          (12, 4),
          (14, 6),
          (16, 6),
          (18, 17),
          (20, 8),
          (22, 8),
          (24, 8),
          (26, 9),
          (28, 8),
          (30, 12),
          (32, 12),
          (34, 10),
          (36, 12),
          (38, 12),
          (40, 8),
          (42, 12),
          (44, 12),
          (46, 10),
          (48, 12),
          (50, 12),
          (52, 14),
          (54, 14),
          (56, 12),
          (58, 14),
          (60, 14),
          (62, 14),
          (64, 14),
          (66, 14),
          (68, 12),
          (70, 14),
          (72, 14),
          (74, 14),
          (76, 14),
          (80, 18),
          (82, 14),
          (90, 18)]

