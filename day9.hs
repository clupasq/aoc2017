import Test.Hspec

combine :: (String, String) -> (String, String) -> (String, String)
combine (a, b) (c, d) = (a ++ c, b ++ d)

-- takes a String input and returns a tuple containing:
--  * the braces
--  * the garbage content
parse :: String -> (String, String)
parse (x:xs) | x `elem` "{}" = ([x], []) `combine` parse xs
             | x == '<'      = parseGarbage xs
             | otherwise     = parse xs
parse     [] = ([], [])

parseGarbage :: String -> (String, String)
parseGarbage ('!':x:xs) = parseGarbage xs
parseGarbage ('>'  :xs) = parse xs
parseGarbage (    x:xs) = ([], [x]) `combine` parseGarbage xs

clean   = fst . parse
garbage = snd . parse

scoreBraces :: String -> Int
scoreBraces = score' 0
  where score'     _     [] = 0
        score' level (x:xs) | x == '{'  = score' (level+1) xs
                            | x == '}'  = level + score' (level-1) xs
                            | otherwise = score' level xs

score = scoreBraces . clean


main = hspec $ do

  describe "Cleaning input" $ do

    it "cleans up characters other than curly braces" $ do
      clean "{}" `shouldBe` "{}"
      clean "{ }" `shouldBe` "{}"
      clean "{{1}, {3}}" `shouldBe` "{{}{}}"

    it "cleans up garbage between angle brackets" $ do
      clean "{<{foo}, {bar}>}" `shouldBe` "{}"

    it "bang! escapes a char inside garbage" $ do
      clean "{<!>}>}" `shouldBe` "{}"
      clean "{{<!!>},{<!!>},{<!!>},{<!!>}}" `shouldBe` "{{}{}{}{}}"


  describe "Computes score" $ do

    it "top level groups all score 1" $ do
      scoreBraces "{}" `shouldBe` 1
      scoreBraces "{}{}" `shouldBe` 2
      scoreBraces "{}{}{}" `shouldBe` 3

    it "nested groups score 1 higher than parents" $ do
      scoreBraces "{{{}}}" `shouldBe` 6
      scoreBraces "{{},{}}" `shouldBe` 5
      scoreBraces "{{{},{},{{}}}}" `shouldBe` 16

    it "can score more complex cases" $ do
      score "{<a>,<a>,<a>,<a>}" `shouldBe` 1
      score "{<{},{},{{}}>}" `shouldBe` 1
      score "{{<ab>},{<ab>},{<ab>},{<ab>}}" `shouldBe` 9
      score "{{<!!>},{<!!>},{<!!>},{<!!>}}" `shouldBe` 9
      score "{{<a!>},{<a!>},{<a!>},{<ab>}}" `shouldBe` 3


  describe "Questions" $ do

    it "solves question #1" $ do
      readFile "input/day9.in" >>= (print . score)

    it "solves question #2" $ do
      readFile "input/day9.in" >>= (print . (length . garbage))

