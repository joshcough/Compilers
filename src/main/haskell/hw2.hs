import Read (SExpr(AtomSym, AtomNum, List), sread)
import TestHelpers
import Test.HUnit

data Exp = Id String | Num Integer | Add Exp Exp | Sub Exp Exp | With String Exp Exp
  deriving (Show, Eq)

parse :: SExpr -> Exp
parse (AtomSym s) = Id s
parse (AtomNum n) = Num n 
parse (List ((AtomSym "+") : xs : ys : [])) = Add (parse xs) (parse ys)
parse (List ((AtomSym "-") : xs : ys : [])) = Sub (parse xs) (parse ys)
parse (List ((AtomSym "with") : (List ((AtomSym x) : e : [])) : b : [])) = With x (parse e) (parse b)
parse _ = error "unexpected token"

eval :: Exp -> Integer
eval e = eval2 e []

eval2 :: Exp -> [(String, Integer)] -> Integer
eval2 (Num i) _ = i
eval2 (Add e1 e2) bs = (eval2 e1 bs) + (eval2 e2 bs)
eval2 (Sub e1 e2) bs = (eval2 e1 bs) - (eval2 e2 bs)
eval2 (With x e body) bs = let v = (eval2 e bs) in eval2 body ((x, v) : bs)
eval2 (Id s) b = lookup s b where 
  lookup s [] = error ("free variable: " ++ s)
  lookup s ((x, v) : xs) = if s == x then v else lookup s xs

--------------------
------ tests -------
--------------------

parseTest :: String -> Exp -> Test
parseTest s expected = makeTest s (parse (sread s)) expected 

evalTest :: String -> Integer -> Test
evalTest s expected = makeTest s (eval (parse (sread s))) expected 

results = runTests
 [
   parseTest "(+ 5 6)" (Add (Num 5) (Num 6)),
   parseTest "(- 5 6)" (Sub (Num 5) (Num 6)),
   evalTest "5" 5,
   evalTest "(+ 5 6)" 11,
   evalTest "(with (x 7) (+ x 9))" 16,
   evalTest "(with (x 7) (with (x 8) x))" 8
 ]

