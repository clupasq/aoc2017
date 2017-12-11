import Test.Hspec

{-

     >---<       >---<       >---<       >---<       >---<       >---<
    /     \     /     \     /     \     /     \     /     \     /     \
---< -4,-1 >---< -3,0  >---<       >---< -1,2  >---<  0,3  >---<  1,4  >
    \     /     \     /     \     /     \     /     \     /     \     /
     >---< -3,-1 >---< -2,0  >---< -1,1  >---<  0,2  >---<  1,3  >---<
    /     \     /     \     /     \     /     \     /     \     /     \
---<       >---< -2,-1 >---< -1,0  >---<  0,1  >---<  1,2  >---<  2,3  >
    \     /     \     /     \     / *** \     /     \     /     \     /
     >---<       >---< -1,-1 >---<  0,0  >---<  1,1  >---<  2,2  >---<
    /     \     /     \     /     \ *** /     \     /     \     /     \
---<       >---< -1,-2 >---<  0,-1 >---<  1,0  >---<  2,1  >---<  3,2  >
    \     /     \     /     \     /     \     /     \     /     \     /
     >---<       >---<  0,-2 >---<  1,-1 >---<  2,0  >---<  3,1  >---<
    /     \     /     \     /     \     /     \     /     \     /     \
---<       >---<       >---<  1,-2 >---<  2,-1 >---<  3,0  >---<  4,1  >
    \     /     \     /     \     /     \     /     \     /     \     /
     >---<       >---<       >---<       >---<       >---<  4,0  >---<

-}

import Data.Char

data Direction = N | NE | SE | S | SW | NW
               deriving (Eq, Show, Read)

type Coords = (Int, Int)

move :: Direction -> Coords -> Coords
move N  (y, x) = (y-1, x+1)
move S  (y, x) = (y+1, x-1)
move SE (y, x) = (y+1, x  )
move SW (y, x) = (y  , x-1)
move NE (y, x) = (y  , x+1)
move NW (y, x) = (y-1, x  )

destination :: Coords -> [Direction] -> Coords
destination = foldl (flip move)

path :: Coords -> [Direction] -> [Coords]
path = scanl (flip move)

distance :: Coords -> Coords -> Int
distance (y1, x1) (y2, x2) | x1 == x2  = ady
                           | y1 == y2  = adx
                           | dy == -dx = adx
                           | dy == dx  = 2 * adx
                           | dx >= 0 && dy >= 0 = dx + dy
                           | dx <  0 && dy <  0 = adx + ady
                           | dx <  0            = 1 + distance (y1, x1) (y2-1, x2+1)
                           | dy <  0            = 1 + distance (y1, x1) (y2+1, x2-1)
  where dy  = y2 - y1
        dx  = x2 - x1
        ady = abs dy
        adx = abs dx

origin :: Coords
origin = (0, 0)

main = hspec $ do

  describe "Hex grid" $ do

    describe "movement" $ do

      it "North" $ do
        move N (0, 0) `shouldBe` (-1, 1)
        move N (3, 1) `shouldBe` (2, 2)

      it "South" $ do
        move S (0, 0) `shouldBe` (1, -1)
        move S (3, 1) `shouldBe` (4, 0)

      it "SouthEast" $ do
        move SE (0, 0) `shouldBe` (1, 0)
        move SE (3, 1) `shouldBe` (4, 1)

      it "SouthWest" $ do
        move SW (0, 0) `shouldBe` (0, -1)
        move SW (3, 1) `shouldBe` (3, 0)

      it "NorthEast" $ do
        move NE (0, 0) `shouldBe` (0, 1)
        move NE (3, 1) `shouldBe` (3, 2)

      it "NorthWest" $ do
        move NW (0, 0) `shouldBe` (-1, 0)
        move NW (3, 1) `shouldBe` (2, 1)


      it "can follow paths to reach a destination" $ do
        destination (0, 0) [NE, NE, NE]         `shouldBe` (0, 3)
        destination (0, 0) [NE, NE, SW, SW]     `shouldBe` (0, 0)
        destination (0, 0) [NE, NE, S, S]       `shouldBe` (2, 0)
        destination (0, 0) [SE, SW, SE, SW, SW] `shouldBe` (2, -3)

    describe "Distance measurement" $ do

      it "same points -> distance = 0" $ do
        distance (0, 0) (0, 0) `shouldBe` 0
        distance (0, 1) (0, 1) `shouldBe` 0

      it "same x coord" $ do
        distance (0, 0) (1, 0) `shouldBe` 1
        distance (9, 3) (1, 3) `shouldBe` 8

      it "same y coord" $ do
        distance (0, 0) (0, 1) `shouldBe` 1
        distance (3, 9) (3, 3) `shouldBe` 6

      it "travelling only vertically" $ do
        distance (0, 0) (1, -1) `shouldBe` 1
        distance (0, 0) (-1, 1) `shouldBe` 1
        distance (2, 1) (1, 2) `shouldBe` 1

      it "travelling only horizontally" $ do
        distance (0, 0) (1, 1) `shouldBe` 2
        distance (0, 0) (3, 3) `shouldBe` 6

      it "both deltas positive" $ do
        distance (0, 0) (4, 1) `shouldBe` 5
        distance (0, 0) (5, 3) `shouldBe` 8

      it "both deltas negative" $ do
        distance (0, 0) (-4, -1) `shouldBe` 5
        distance (0, 0) (-5, -3) `shouldBe` 8

      it "mixed deltas" $ do
        distance (0, 0) (2, -1) `shouldBe` 2
        distance (0, 0) (-2, 1) `shouldBe` 2



    describe "Questions" $ do

      it "answers question #1" $ do
        input <- readFile "input/day11.in"
        let dirs = map read $ words $ map toUpper input
        let final = destination origin dirs
        print $ "Final coords: " ++ (show final)
        print $ "Distance:     " ++ (show $ distance origin final)

      it "answers question #1" $ do
        -- todo: make distance calculation faster (currently takes ~28s)
        input <- readFile "input/day11.in"
        let dirs = map read $ words $ map toUpper input
        let visited = path origin dirs
        let farthest = maximum (map (distance origin) visited)
        print $ "Farthest:     " ++ (show $ farthest)





