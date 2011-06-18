import Read (SExpr(Symbol, List), sread)
import TestHelpers
import Test.HUnit

data Exp = StrLit String | Concat Exp Exp | RestAfter Exp Exp
  deriving (Show, Eq)

parser :: SExpr -> Exp
parser (Symbol s) = StrLit s
parser (List (xs : (Symbol "&") : ys : [])) = Concat (parser xs) (parser ys)
parser (List (xs : (Symbol "@") : ys : [])) = RestAfter (parser xs) (parser ys)
parser _ = error "unexpected token"

interpreter :: Exp -> String
interpreter (StrLit s) = s
interpreter (Concat l r) = (interpreter l) ++ (interpreter r)
interpreter (RestAfter l r) = finder (interpreter r) (interpreter l) where 
  finder :: String -> String -> String
  finder findme instring =
    if (length findme) > (length instring) then error (findme ++ " not found")
    else if findme == (take (length findme) instring) then drop (length findme) instring
    else finder findme (drop 1 instring)

--------------------
------ tests -------
--------------------

evalTest :: String -> String -> Test
evalTest s expected = makeTest s (interpreter (parser (sread s))) expected 

results = runTests
 [
   evalTest "hello" "hello",
   evalTest "(hello & world)" "helloworld",
   evalTest "((hello & world) @ wo)" "rld",
   evalTest "(((hello & world) @ ll) @ w)" "orld"
 ]

