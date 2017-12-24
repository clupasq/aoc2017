import Test.Hspec
import Debug.Trace

import qualified Data.Map as Map

data Direction = N | E | S | W
               deriving (Eq, Show, Enum, Bounded)

data CellState = Clean | Weakened | Infected | Flagged
               deriving (Eq, Show, Enum, Bounded)

type Coord = (Int, Int)
type World = Map.Map Coord CellState

type Virus = (Coord, Direction)
type State = (Virus, World, Int)

right :: Direction -> Direction
right W = N
right d = succ d

left :: Direction -> Direction
left N = W
left d = pred d

nextState :: CellState -> CellState
nextState Flagged = Clean
nextState       x = succ x

changeDir :: CellState -> Direction -> Direction
changeDir Clean    = left
changeDir Weakened = id
changeDir Infected = right
changeDir Flagged  = right.right

advance :: Direction -> Coord -> Coord
advance N (y, x) = (y-1, x  )
advance E (y, x) = (y  , x+1)
advance S (y, x) = (y+1, x  )
advance W (y, x) = (y  , x-1)

parse :: String -> World
parse xs = foldr parseRow Map.empty numberedRows
  where startIndex = -(length $ lines xs) `div` 2
        numberedRows = zip [startIndex..] $ lines xs
        parseRow (y, row) m = foldr addCell m $ zip [startIndex..] row
          where addCell (x, c) m' | c == '.'  = m'
                                  | otherwise = Map.insert (y, x) Infected m'

step :: State -> State
step ((coord, dir), world, i) = ((coord', dir'), updatedWorld, i')
  where cell         = Map.findWithDefault Clean coord world
        infected     = case cell of
                            Infected -> True
                            _        -> False
        dir'         = if infected
                        then right dir
                        else left  dir
        coord'       = advance dir' coord
        updatedWorld = Map.insert coord (if infected then Clean else Infected) world
        i'           = if infected then i else i + 1


step' :: State -> State
step' ((coord, dir), world, i) = ((coord', dir'), updatedWorld, i')
  where cell         = Map.findWithDefault Clean coord world
        cell'        = nextState cell
        dir'         = changeDir cell dir
        coord'       = advance dir' coord
        updatedWorld = Map.insert coord cell' world
        i'           = case cell' of
                            Infected -> i + 1
                            _        -> i





tests = hspec $ describe "Sporifica Virus" $ do

  let sampleMap = "..#\n\
                  \#..\n\
                  \..."

  let initialWorld = parse sampleMap
  let initialVirus = ((0, 0), N)
  let initialState = (initialVirus, initialWorld, 0)

  it "can parse the map" $ do
    initialWorld `shouldBe` Map.fromList [((-1,  1), Infected),
                                          (( 0, -1), Infected) ]

  it "can count infections" $ do
    let after10000 = (iterate step initialState) !! 10000
    let (v, w, i) = after10000
    i `shouldBe` 5587

  it "Question #1" $ do
    mapText <- readFile "input/day22.in"
    let actualMap = parse mapText
    let after10000 = (iterate step (initialVirus, actualMap, 0)) !! 10000
    let (v, w, s) = after10000
    print s

  it "can count infections 2" $ do
    let after100 = (iterate step' initialState) !! 100
    let (v, w, i) = after100
    i `shouldBe` 26

  it "Question #2" $ do
    -- slow, stackoverflow. Ruby...
    mapText <- readFile "input/day22.in"
    let actualMap = parse mapText
    let muchLater = (iterate step (initialVirus, actualMap, 0)) !! 10000000
    let (v, w, s) = muchLater
    print s






main = do
  tests
