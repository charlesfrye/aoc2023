import Lib
import Test.HUnit

main :: IO Counts
main = runTestTT tests

tests :: Test
tests = TestList [TestLabel "tests" testFirst, TestLabel "tests" testSecond]

testFirst :: Test
testFirst = TestCase $ do
  fileContents <- readFile "test.txt"
  let inputs = parseInput fileContents
  assertEqual "for first test problem, output is" [[1, 2], [3, 8], [1, 5], [7, 7]] (map (extractCalVal . tokenize) inputs)

testSecond :: Test
testSecond = TestCase $ do
  fileContents <- readFile "test2.txt"
  let inputs = parseInput fileContents
  assertEqual "for second test problem, output is" [[2, 9], [8, 3], [1, 3], [2, 4], [4, 2], [1, 4], [7, 6]] (map (extractCalVal . tokenize') inputs)
