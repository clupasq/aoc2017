
import Data.List

type Id           = Int
type Triplet      = (Int, Int, Int)
type Position     = Triplet
type Velocity     = Triplet
type Acceleration = Triplet

data Particle = Particle Id Position Velocity Acceleration
              deriving (Eq, Show)


plus :: Triplet -> Triplet -> Triplet
plus (x, y, z) (p, q, r) = (p+x, q+y, r+z)

sumAcc :: Particle -> Int
sumAcc (Particle _ _ _ (ax, ay, az)) = sum [abs i | i <- [ax, ay, az]]

update :: Particle -> Particle
update (Particle id pos velo acc) = Particle id pos' velo' acc
  where velo' = velo `plus` acc
        pos'  = pos  `plus` velo'

updateWorld :: [Particle] -> [Particle]
updateWorld = map update . removeColliding

pos :: Particle -> Position
pos (Particle _ p _ _) = p

-- assumes that the input list is sorted
linearNubBy :: Eq b => (a -> b) -> [a] -> [a]
linearNubBy f (x:y:xs) | f x == f y =     rest
                       | otherwise  = x : rest
  where rest = linearNubBy f (y:xs)
linearNubBy _        x = x

removeColliding :: [Particle] -> [Particle]
removeColliding ps = concat $ filter ((==1).length) $ groupBy (\x y -> pos x == pos y) $ sortOn pos ps

mkParticle :: (String, Id) -> Particle
mkParticle (xs, id) = Particle id pos velo acc
  where [x, y, z, vx, vy, vz, ax, ay, az] = map read $ words xs
        pos  = (x, y, z)
        velo = (vx, vy, vz)
        acc  = (ax, ay, az)



readParticles :: IO [Particle]
readParticles = do
  text <- readFile "input/day20.in"
  return $ map mkParticle $ zip (lines text) [0..]


main = do
  ps <- readParticles

  let slowestGrowing = head $ sortOn sumAcc ps

  putStrLn "Slowest growing:"
  -- the one that eventually remains closest to the origin
  -- is the one with the least absolute acceleration
  -- (because acceleration trumps velocity, which trumps
  -- initial position).
  print slowestGrowing


  let counts = map length $ take 100 $ iterate updateWorld ps
  print $ counts

  print "done"

