--  <FunDef> = {deffun {<id> <id>*} <F1WAE>}
--  <F1WAE> = <number>
--          | {+ <F1WAE> <F1WAE>}
--          | {- <F1WAE> <F1WAE>}
--          | {with {<id> <F1WAE>} <F1WAE>}
--          | <id>
--          | {<id> <F1WAE>*}
--          | {rec {<id> <F1WAE>}*}
--          | {get <F1WAE> <id>}

import Read (SExpr(AtomSym, AtomNum, List), sread)
import TestHelpers
import Test.HUnit
import Data.List
import Data.Maybe

data F1WAE = 
  Id String | Num Integer | Add F1WAE F1WAE | Sub F1WAE F1WAE | With String F1WAE F1WAE | 
  App String [F1WAE] | Rec [(String, F1WAE)] | Get F1WAE String
  deriving (Show, Eq)

data FunDef = FunDef String [String] F1WAE deriving (Show, Eq)
data Value = VNum Integer | VRec [(String, Value)] deriving (Show, Eq)

parse :: SExpr -> F1WAE
parse (AtomSym s) = Id s
parse (AtomNum n) = Num n 
parse (List ((AtomSym "+") : xs : ys : [])) = Add (parse xs) (parse ys)
parse (List ((AtomSym "-") : xs : ys : [])) = Sub (parse xs) (parse ys)
parse (List ((AtomSym "with") : (List ((AtomSym x) : e : [])) : b : [])) = With x (parse e) (parse b)
parse (List ((AtomSym "rec") : fields)) = parseRec fields where 
  parseRec fields = if containsDups (map name fields)
    then (error "duplicate fields")
    else Rec (map (\f -> ((name f), (parse (body f)))) fields) 
  containsDups xs = length xs > length (nub xs)
  name (List ((AtomSym x) : _ )) = x
  name x = error "bad rec: " ++ (show x)
  body (List (_ : b : [])) = b
  body x = (error "bad rec")
parse (List ((AtomSym "get") : r : (AtomSym i) : [])) = Get (parse r) i
parse (List ((AtomSym f) : args)) = App f (map parse args)
parse _ = error "unexpected token"

parseDef :: SExpr -> FunDef
parseDef (List ((AtomSym "deffun") : (List ((AtomSym name) : args)) : body : [])) = 
  FunDef name (map get args) (parse body) where
  get (AtomSym s) = s

eval :: F1WAE -> Value
eval e = eval2 e []

eval2 :: F1WAE -> [(String, Value)] -> Value
eval2 (Num i) _ = VNum i
eval2 (Add e1 e2) bs = math (+) (eval2 e1 bs) (eval2 e2 bs)
eval2 (Sub e1 e2) bs = math (-) (eval2 e1 bs) (eval2 e2 bs)
eval2 (With x e body) bs = let v = (eval2 e bs) in eval2 body ((x, v) : bs)
eval2 (Id s) b = lookup s b where 
  lookup s [] = error ("free variable: " ++ s)
  lookup s ((x, v) : xs) = if s == x then v else lookup s xs
eval2 (Rec fields) bs = VRec (map (\f -> ((fst f), (eval2 (snd f) bs))) fields)
--eval2 (Get r) bs = 

math f (VNum l) (VNum r) = VNum (f l r) 
math _ _ _ = (error "cant do math on records")

--------------------
------ tests -------
--------------------

parseTest s expected = makeTest s (parse (sread s)) expected 
parseDefTest s expected = makeTest s (parseDef (sread s)) expected 
evalTest s expected = makeTest s (eval (parse (sread s))) expected 

results = runTests
 [
   parseTest "(+ 5 6)" (Add (Num 5) (Num 6)),
   parseTest "(- 5 6)" (Sub (Num 5) (Num 6)),
   parseDefTest "(deffun (f x y) (+ x y))" (FunDef "f" ["x","y"] (Add (Id "x") (Id "y"))),
   parseDefTest "(deffun (f) 7)" (FunDef "f" [] (Num 7)),
   evalTest "5" (VNum 5),
   evalTest "(+ 5 6)" (VNum 11),
   evalTest "(with (x 7) (+ x 9))" (VNum 16),
   evalTest "(with (x 7) (with (x 8) x))" (VNum 8)
 ]

-- (test (interp (parse '{f 1 2})
--                (list (parse-defn '{deffun {f x y} {+ x y}})))
--        3)
--  (test (interp (parse '{+ {f} {f}})
--                (list (parse-defn '{deffun {f} 5})))
--        10)
--  (test/exn (interp (parse '{f 1})
--                    (list (parse-defn '{deffun {f x y} {+ x y}})))
--            "wrong arity")

