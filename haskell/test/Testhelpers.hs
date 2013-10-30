module TestHelpers (makeTest, runTests) where

import Test.HUnit

makeTest :: (Eq a, Show a) => String -> a -> a -> Test
makeTest name expected actual = TestLabel name (TestCase (assertEqual name expected actual))

runTests ts = runTestTT (TestList (ts))

