import Read (SExpr(AtomSym, List), sread)
import TestHelpers
import Test.HUnit

data Exp = StrLit String | Concat Exp Exp | RestAfter Exp Exp
  deriving (Show, Eq)

parse :: SExpr -> Exp
parse (AtomSym s) = StrLit s
parse (List (xs : (AtomSym "&") : ys : [])) = Concat (parse xs) (parse ys)
parse (List (xs : (AtomSym "@") : ys : [])) = RestAfter (parse xs) (parse ys)
parse _ = error "unexpected token"

eval :: Exp -> String
eval (StrLit s) = s
eval (Concat l r) = (eval l) ++ (eval r)
eval (RestAfter l r) = finder (eval r) (eval l) where 
  finder :: String -> String -> String
  finder findme instring =
    if (length findme) > (length instring) then error (findme ++ " not found")
    else if findme == (take (length findme) instring) then drop (length findme) instring
    else finder findme (drop 1 instring)

--------------------
------ tests -------
--------------------

evalTest :: String -> String -> Test
evalTest s expected = makeTest s (eval (parse (sread s))) expected 

results = runTests
 [
   evalTest "hello" "hello",
   evalTest "(hello & world)" "helloworld",
   evalTest "((hello & world) @ wo)" "rld",
   evalTest "(((hello & world) @ ll) @ w)" "orld"
 ]

