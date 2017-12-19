import Test.Hspec

import qualified Data.Map as M
import Data.Char (isLetter)

type Coords = (Int, Int)
type Maze   = M.Map Coords Char
data Direction = N | S | E | W

withIndex :: [a] -> [(Int, a)]
withIndex = zip [0..]

mkMaze :: String -> Maze
mkMaze xs = foldr mapLine M.empty (withIndex $ lines xs)
  where mapLine (y, cs) m = foldr mapChar m (withIndex cs)
          where mapChar (x, c) m = M.insert (y, x) c m

data TraversalState = TraversalState { coords    :: Coords,
                                       direction :: Direction,
                                       message   :: String,
                                       done      :: Bool
                                     }

getMazeCell :: Coords -> Maze -> Char
getMazeCell = M.findWithDefault ' '

getNext :: Coords -> Direction -> Coords
getNext (y, x) S = (y+1, x  )
getNext (y, x) W = (y  , x-1)
getNext (y, x) N = (y-1, x  )
getNext (y, x) E = (y  , x+1)

getPossibleDirs :: Direction -> [Direction]
getPossibleDirs N = [N, W, E]
getPossibleDirs S = [S, W, E]
getPossibleDirs W = [W, S, N]
getPossibleDirs E = [E, S, N]


extractMessage :: String -> String
extractMessage = reverse . filter isLetter

traverseMaze :: Maze -> String
traverseMaze m = message $ head $ dropWhile (not.done) $ iterate step initial
  where entry = head [ (y,x) | ((y,x), c) <- M.toList m, y==0, c=='|' ]
        initial = TraversalState { coords    = entry,
                                   direction = S,
                                   message   = "",
                                   done      = False
                                 }
        step state = state { coords    = coords',
                             direction = direction',
                             message   = crt : (message state),
                             done      = null possibs
                           }
          where possibs = [(c, d) | d <- getPossibleDirs (direction state),
                                    c <- [getNext (coords state) d],
                                    getMazeCell c m /= ' ']
                coords'    = if null possibs
                                then coords state
                                else (fst $ head possibs)
                direction' = if null possibs
                                then direction state
                                else (snd $ head possibs)
                crt        = getMazeCell (coords state) m



sampleMaze =  "     |          \n\
              \     |  +--+    \n\
              \     A  |  C    \n\
              \ F---|----E|--+ \n\
              \     |  |  |  D \n\
              \     +B-+  +--+ \n\
              \                "

tests = hspec $ describe "Maze" $ do

  it "can solve simple maze" $ do
    let maze = mkMaze sampleMaze
    (extractMessage . traverseMaze) maze `shouldBe` "ABCDEF"

main = do
  tests
  text <- readFile "input/day19.in"
  let maze = mkMaze text
  let path = traverseMaze maze
  putStrLn (extractMessage path)
  putStrLn $ (show $ length path) ++ " steps."
  print "Done."

