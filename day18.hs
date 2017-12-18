import Test.Hspec

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
            | Stop
  deriving (Eq, Show)

type Registers = M.Map Reg Int

data CPU = CPU { regs         :: Registers,
                 stack        :: [Int],
                 instructions :: [Instruction],
                 pointer      :: Int,
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
            in cpu' { stack = x : (stack cpu) }
      (Rcv reg     ) ->
        if ((eval cpu $ Left reg) > 0)
           then cpu' { lastResult = Received s, stack = ss }
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
        (s:ss) = stack cpu


readInstructions :: IO [Instruction]
readInstructions = do
  text <- readFile "input/day18.in"
  return $ map parse $ lines text

mkCPU :: [Instruction] -> CPU
mkCPU is = CPU { instructions = is,
                 pointer      = 0,
                 regs         = M.empty,
                 stack        = [],
                 lastResult   = None
               }

hasReceived :: CPU -> Bool
hasReceived cpu = case lastResult cpu of
                        (Received _) -> True
                        _            -> False









tests = hspec $ describe "Duet" $ do

  let testCPU = CPU { regs         = M.fromList [('x', 1),
                                           ('y', 2),
                                           ('z', 7)],
                instructions = [],
                stack        = [],
                pointer      = 0,
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


main = do
  tests
  is <- readInstructions
  let cpu = mkCPU is

  putStrLn "Question #1"
  let state = head [state | state <- iterate exec cpu, hasReceived state]
  print $ lastResult state





