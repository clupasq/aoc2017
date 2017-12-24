import Test.Hspec
import Debug.Trace

import Data.Char
import qualified Data.Map as M


type Reg = Char
type Expr = Either Reg Int

data Instruction = Set Reg Expr
                 | Sub Reg Expr
                 | Mul Reg Expr
                 | Mod Reg Expr
                 | Jnz Expr Expr
                 deriving (Eq, Show)

data Result = None
            | Stop
  deriving (Eq, Show)

type Registers = M.Map Reg Int

data CPU = CPU { name         :: String,
                 regs         :: Registers,
                 queue        :: [Int],
                 instructions :: [Instruction],
                 pointer      :: Int,
                 mulCount     :: Int,
                 lastResult   :: Result }
  deriving (Eq, Show)

parse :: String -> Instruction
parse xs =
  case w of
       "set" -> Set (parseReg p1) (parseExpr p2)
       "sub" -> Sub (parseReg p1) (parseExpr p2)
       "mul" -> Mul (parseReg p1) (parseExpr p2)
       "mod" -> Mod (parseReg p1) (parseExpr p2)
       "jnz" -> Jnz (parseExpr p1)(parseExpr p2)

  where ( w:ws) = words xs
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
      (Sub reg expr) ->
        cpu' { regs = M.insert reg ((eval cpu $ Left reg) -     (eval cpu expr)) rs }
      (Mul reg expr) ->
        cpu' { regs = M.insert reg ((eval cpu $ Left reg) *     (eval cpu expr)) rs,
               mulCount = 1 + mulCount cpu }
      (Mod reg expr) ->
        cpu' { regs = M.insert reg ((eval cpu $ Left reg) `mod` (eval cpu expr)) rs }
      (Jnz ex1 ex2)  ->
        if (eval cpu ex1) /= 0
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
  text <- readFile "input/day23.opti.in"
  return $ map parse $ lines text


defaultRegs :: M.Map Char Int
defaultRegs = M.fromList [ (c, 0) | c <- ['a'..'h']]

defaultDebugRegs :: M.Map Char Int
defaultDebugRegs = M.insert 'a' 1 defaultRegs

mkCPU :: [Instruction] -> CPU
mkCPU is = CPU { instructions = is,
                 pointer      = 0,
                 regs         = defaultRegs,
                 queue        = [],
                 lastResult   = None,
                 name         = "",
                 mulCount     = 0
               }

crtInstruction :: CPU -> Instruction
crtInstruction cpu = (instructions cpu) !! (pointer cpu)

traceExec :: CPU -> CPU
traceExec c = trace (show (pointer c, M.toList $ regs c)) exec c

tests = hspec $ describe "Duet" $ do

  let testCPU = CPU { regs         = M.fromList [ ('x', 1),
                                                  ('y', 2),
                                                  ('z', 7)],
                      name         = "test",
                      instructions = [],
                      queue        = [],
                      pointer      = 0,
                      mulCount     = 0,
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
      parse "sub a 2" `shouldBe` Sub 'a' (Right 2)
      parse "sub a b" `shouldBe` Sub 'a' (Left 'b')
      parse "mul a a" `shouldBe` Mul 'a' (Left 'a')
      parse "set a 0" `shouldBe` Set 'a' (Right 0)
      parse "jnz a -1" `shouldBe` Jnz (Left 'a') (Right (-1))
      parse "jnz 1 -1" `shouldBe` Jnz (Right  1) (Right (-1))
      parse "set a p" `shouldBe` Set 'a' (Left 'p')


stopped :: CPU -> Bool
stopped c = lastResult c == Stop

question1 :: IO ()
question1 = do
  is <- readInstructions
  let cpu = mkCPU is
  let final = until stopped exec cpu
  print $ mulCount final

debugging :: IO ()
debugging = do
  is <- readInstructions
  let cpu = (mkCPU is) { regs = defaultDebugRegs }
  let final = head $ drop 100000 $ iterate traceExec cpu
  print $ M.lookup 'h' (regs final)


question2 :: IO ()
question2 = do
  is <- readInstructions
  let cpu = (mkCPU is) { regs = defaultDebugRegs }
  let final = until (\c -> lastResult c == Stop) exec cpu
  print $ M.lookup 'h' (regs final)

main = do
  tests
  -- question1
  debugging
  -- question2


