import Test.Hspec

import qualified Data.Map as M
import Data.Maybe


type Register = String

data Comparison = Eq | Neq | Gt | Gte | Lt | Lte
                deriving (Eq, Show)

type Condition = (String, Comparison, Int)

data Instruction = Instruction Register Int Condition deriving (Eq, Show)

type CPU = M.Map Register Int

operation :: Comparison -> (Int -> Int -> Bool)
operation Eq  = (==)
operation Neq = (/=)
operation Gt  = (>)
operation Gte = (>=)
operation Lt  = (<)
operation Lte = (<=)


getVal :: CPU -> Register -> Int
getVal cpu r = fromMaybe 0 (M.lookup r cpu)

eval :: CPU -> Condition -> Bool
eval cpu (reg, comp, val) = op x val
  where op = operation comp
        x  = getVal cpu reg

exec :: CPU -> Instruction -> CPU
exec cpu (Instruction reg incr cond) | eval cpu cond = M.insert reg newval cpu
                                     | otherwise     = cpu
  where newval = getVal cpu reg + incr


parse :: String -> Instruction
parse str = Instruction reg incr (condreg, comp, val)
  where [reg, op, incdecstr, _, condreg, compstr, valstr] = words str
        incdec = read incdecstr
        incr   = if op == "dec" then (-incdec) else incdec
        val    = read valstr
        comp   = case compstr of
                      "==" -> Eq
                      "!=" -> Neq
                      ">"  -> Gt
                      ">=" -> Gte
                      "<"  -> Lt
                      "<=" -> Lte


highest :: CPU -> Int
highest cpu | M.null cpu = 0
            | otherwise  = maximum (M.elems cpu)


testCPU :: CPU
testCPU = M.fromList [("a", 10),
                      ("b", 20)]

getInstructions :: IO [Instruction]
getInstructions = do
  text <- readFile "input/day8.in"
  return $ map parse $ lines text

main = hspec $ do

  describe "CPU" $ do

    it "can parse instructions" $ do
      parse "d dec 461 if oiy <= 1" `shouldBe`
                          Instruction "d" (-461) ("oiy", Lte, 1)
      parse "eai inc 302 if pce >= -6317" `shouldBe`
                          Instruction "eai" 302 ("pce", Gte, (-6317))



    it "can read register values" $ do
      getVal testCPU "a" `shouldBe` 10
      getVal testCPU "b" `shouldBe` 20

    it "new registers start at 0" $ do
      getVal testCPU "unknown_reg" `shouldBe` 0

    it "can evaluate conditions" $ do
      eval testCPU ("a", Gt ,  9) `shouldBe` True
      eval testCPU ("a", Lt , 10) `shouldBe` False
      eval testCPU ("a", Lte, 10) `shouldBe` True
      eval testCPU ("a", Gte, 11) `shouldBe` False
      eval testCPU ("a", Gte, 10) `shouldBe` True
      eval testCPU ("a", Eq , 10) `shouldBe` True
      eval testCPU ("a", Neq, 10) `shouldBe` False
      eval testCPU ("b", Eq , 10) `shouldBe` False
      eval testCPU ("b", Neq, 10) `shouldBe` True

    describe "instruction execution" $ do

      it "registers are affected" $ do
        let instr = Instruction "a" 1 ("a", Gt, 0)
        let cpu'  = exec testCPU instr
        getVal cpu' "a" `shouldBe` 11

      it "registers are unchanged if condition is false" $ do
        let instr = Instruction "a" 1 ("a", Gt, 100000)
        let cpu'  = exec testCPU instr
        cpu' `shouldBe` testCPU


    describe "questions" $ do

      it "answers Q1" $ do
        instrs <- getInstructions
        let cpu = M.fromList []
        let finalState = foldl exec cpu instrs
        putStrLn "The highest value after all instructions:"
        print $ highest finalState

      it "answers Q2" $ do
        instrs <- getInstructions
        let cpu = M.fromList []
        let (_, maxval) = foldl step (cpu, 0) instrs
                            where step (c, oldmax) i = let c'     = exec c i
                                                           newmax = max oldmax (highest c')
                                                       in (c', newmax)
        putStrLn "The highest value ever:"
        print $ maxval







