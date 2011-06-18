module Read (SExpr(Symbol,List), sread) where

import TestHelpers
import Test.HUnit

data SExpr = Symbol String | List [SExpr]
  deriving (Show, Eq)

sread :: String -> SExpr
sread s = let (sexpr, _) = readWithRest s in sexpr

readL :: String -> SExpr -> (SExpr,String)
readL (')' : tail) (List acc) = (List acc, tail)
readL (x : xs) (List acc) = let (next, rest) = readWithRest(x : xs) in readL rest (List (acc ++ [next]))
readL _ _ = error "unterminated list"

readChars :: String -> String -> (SExpr, String)
readChars (' ' : tail) acc = (Symbol acc, [' '] ++ tail)
readChars (')' : tail) acc = (Symbol acc, [')'] ++tail)
readChars ('(' : tail) acc = (Symbol acc, ['('] ++tail)
readChars (c : tail) acc = readChars tail (acc ++ [c])
readChars [] acc = (Symbol acc, [])

readWithRest :: String -> (SExpr,String)
readWithRest (' ' : tail) = readWithRest tail
readWithRest ('(' : tail) = readL tail (List [])
--readWithRest ('"' : tail) = readStringLit tail ['"']
readWithRest (c : tail) = readChars (c : tail) []

--readStringLit :: String -> String -> (SExpr, String)
--readStringLit ('"' : tail) acc = (StringLit (acc ++ ['"']), tail)
--readStringLit (c : tail) acc = readStringLit tail (acc ++ [c])

--------------------
------ tests -------
--------------------

results = runTests
  [
    makeTest "7" (sread "7") (Symbol "7"),
    makeTest "(7)" (sread ("(7)")) (List [Symbol "7"])
  ]
