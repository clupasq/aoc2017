import Test.Hspec
import Debug.Trace

import Data.Char
import qualified Data.Map as M


type Reg = Char
type Expr = Either Reg Int

data Instruction = Set Reg Expr
                 | Add Reg Expr
                 | Mul Reg Expr
                 | Mod Reg Expr
                 | Snd Reg
                 | Rcv Reg
                 | Jgz Expr Expr
                 deriving (Eq, Show)

data Result = None
            | Send Int
            | Received Int
            | WaitSnd
            | Stop
  deriving (Eq, Show)

type Registers = M.Map Reg Int

data CPU = CPU { name         :: String,
                 regs         :: Registers,
                 queue        :: [Int],
                 instructions :: [Instruction],
                 pointer      :: Int,
                 sndCount     :: Int,
                 lastResult   :: Result }
  deriving (Eq, Show)

parse :: String -> Instruction
parse xs =
  case w of
       "set" -> Set (parseReg p1) (parseExpr p2)
       "add" -> Add (parseReg p1) (parseExpr p2)
       "mul" -> Mul (parseReg p1) (parseExpr p2)
       "mod" -> Mod (parseReg p1) (parseExpr p2)
       "snd" -> Snd (parseReg p1)
       "rcv" -> Rcv (parseReg p1)
       "jgz" -> Jgz (parseExpr p1)(parseExpr p2)

  where (w:ws) = words xs
        (p1:ps) = ws
        (p2: _) = ps


parseInt :: String -> Int
parseInt = read

parseReg :: String -> Char
parseReg = head . dropWhile (isSpace)

parseExpr :: String -> Expr
parseExpr xs | isNumber  = Right (parseInt xs)
             | otherwise = Left (parseReg xs)
  where isNumber = any isDigit xs

eval :: CPU -> Expr -> Int
eval cpu (Left  reg) = M.findWithDefault 0 reg (regs cpu)
eval   _ (Right val) = val

exec :: CPU -> CPU
exec cpu
  | noinstruction = cpu { lastResult = Stop }
  | otherwise = case instr of
      (Set reg expr) ->
        cpu' { regs = M.insert reg  (eval cpu expr) rs }
      (Add reg expr) ->
        cpu' { regs = M.insert reg ((eval cpu $ Left reg) +     (eval cpu expr)) rs }
      (Mul reg expr) ->
        cpu' { regs = M.insert reg ((eval cpu $ Left reg) *     (eval cpu expr)) rs }
      (Mod reg expr) ->
        cpu' { regs = M.insert reg ((eval cpu $ Left reg) `mod` (eval cpu expr)) rs }
      (Snd reg     ) ->
        let x = eval cpu $ Left reg
            in cpu' { queue = x : (queue cpu) }
            -- in this case, the queue is really a stack...
            -- ... but in the second part it *is* a queue
      (Rcv reg     ) ->
        if ((eval cpu $ Left reg) > 0)
           then cpu' { lastResult = Received q, queue = qq }
           else cpu'
      (Jgz ex1 ex2)  ->
        if (eval cpu ex1) > 0
           then cpu' { pointer = p + (eval cpu ex2) }
           else cpu'
  where p = pointer cpu
        is = instructions cpu
        noinstruction = p < 0 || p >= (length is)
        instr = is !! p
        cpu' = cpu { pointer = (pointer cpu + 1), lastResult = None }
        rs   = regs cpu
        (q:qq) = queue cpu


readInstructions :: IO [Instruction]
readInstructions = do
  text <- readFile "input/day18.in"
  return $ map parse $ lines text

mkCPU :: [Instruction] -> CPU
mkCPU is = CPU { instructions = is,
                 pointer      = 0,
                 regs         = M.empty,
                 queue        = [],
                 lastResult   = None,
                 name         = "",
                 sndCount     = 0
               }

hasReceived :: CPU -> Bool
hasReceived cpu = case lastResult cpu of
                        (Received _) -> True
                        _            -> False


crtInstruction :: CPU -> Instruction
crtInstruction cpu = (instructions cpu) !! (pointer cpu)

appendToQueue :: CPU -> Int -> CPU
appendToQueue cpu i = cpu { queue = queue cpu ++ [i]}

incrSnd :: CPU -> CPU
incrSnd cpu = cpu { sndCount = sndCount cpu + 1 }


deadlocked :: (CPU, CPU) -> Bool
deadlocked (cpu0, cpu1) = waiting cpu0 && waiting cpu1
  where waiting c = lastResult c == WaitSnd

execDuet :: (CPU, CPU) -> (CPU, CPU)
execDuet (cpu0, cpu1) =
  case crtInstruction cpu0 of
    (Snd reg) -> let x = eval cpu0 $ Left reg
                     in (appendToQueue cpu1 x, incrSnd cpu0')
    (Rcv reg) -> case queue cpu0 of
                    (q:qq) -> (cpu0' { queue = qq,
                                       regs  = M.insert reg q (regs cpu0)
                                     }, cpu1)
                    []     -> (cpu1, cpu0 { lastResult = WaitSnd })
    _         -> (exec cpu0, cpu1)
  where cpu0' = cpu0 { pointer = pointer cpu0 + 1, lastResult = None}




tests = hspec $ describe "Duet" $ do

  let testCPU = CPU { regs         = M.fromList [ ('x', 1),
                                                  ('y', 2),
                                                  ('z', 7)],
                      name         = "test",
                      instructions = [],
                      queue        = [],
                      pointer      = 0,
                      sndCount     = 0,
                      lastResult   = None
                    }

  describe "Parsing" $ do

    describe "Tokens" $ do
      it "can parse Int values" $ do
        parseInt " 234" `shouldBe` 234
        parseInt " -234" `shouldBe` -234

      it "can parse Reg values" $ do
        parseReg "a" `shouldBe` 'a'
        parseReg " expr" `shouldBe` 'e'

      it "can parse expressions" $ do
        parseExpr "a" `shouldBe` Left 'a'
        parseExpr " a " `shouldBe` Left 'a'
        parseExpr " 14" `shouldBe` Right 14
        parseExpr "14" `shouldBe` Right 14

    it "Full instructions" $ do
      parse "set a 1" `shouldBe` Set 'a' (Right 1)
      parse "add a 2" `shouldBe` Add 'a' (Right 2)
      parse "add a b" `shouldBe` Add 'a' (Left 'b')
      parse "mul a a" `shouldBe` Mul 'a' (Left 'a')
      parse "mod a 5" `shouldBe` Mod 'a' (Right 5)
      parse "snd a"   `shouldBe` Snd 'a'
      parse "set a 0" `shouldBe` Set 'a' (Right 0)
      parse "rcv a"   `shouldBe` Rcv 'a'
      parse "jgz a -1" `shouldBe` Jgz (Left 'a') (Right (-1))
      parse "jgz 1 -1" `shouldBe` Jgz (Right  1) (Right (-1))
      parse "set a p" `shouldBe` Set 'a' (Left 'p')


debugMsg cpu = (name cpu, crtInstruction cpu)

traceExecDuet :: (CPU, CPU) -> (CPU, CPU)
traceExecDuet (x, y) = execDuet $ trace (show (debugMsg x, debugMsg y)) (x, y)

main = do
  tests
  is <- readInstructions
  let cpu = mkCPU is

  putStrLn "Question #1"
  let state = head [state | state <- iterate exec cpu, hasReceived state]
  print $ lastResult state


  putStrLn "Question #2"
  let cpu0 = cpu { name = "cpu0", regs = M.fromList [('p', 0)]}
  let cpu1 = cpu { name = "cpu1", regs = M.fromList [('p', 1)]}

  let final = until deadlocked traceExecDuet (cpu0, cpu1)
  let (x, y) = final
  putStrLn $ (name x) ++ " sent " ++ (show $ sndCount x) ++ " times."
  putStrLn $ (name y) ++ " sent " ++ (show $ sndCount y) ++ " times."

  putStrLn "Done"





