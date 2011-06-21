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

data FunDef = FunDef { name :: String,  args :: [String], body :: F1WAE } deriving (Show, Eq)
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
eval e = eval2 e [] []

eval2 :: F1WAE -> [FunDef] -> [(String, Value)] -> Value
eval2 (Num i) _ _ = VNum i
eval2 (Add e1 e2) fs bs = math (+) (eval2 e1 fs bs) (eval2 e2 fs bs)
eval2 (Sub e1 e2) fs bs = math (-) (eval2 e1 fs bs) (eval2 e2 fs bs)
eval2 (With x e body) fs bs = let v = (eval2 e fs bs) in eval2 body fs ((x, v) : bs)
eval2 (Id s) fs bs = lookup s bs where 
  lookup s [] = error ("free variable: " ++ s)
  lookup s ((x, v) : xs) = if s == x then v else lookup s xs
eval2 (Rec fields) fs bs = VRec (map (\f -> ((fst f), (eval2 (snd f) fs bs))) fields)
eval2 (Get r x) fs bs = findInRec (eval2 r fs bs) x where 
  findInRec (VRec fields) x = 
    snd (fromMaybe (error "no such field") (find (\f -> (fst f) == x) fields))
eval2 (App fname fargs) fs bs =
  let f = fromMaybe (error "no such function") (find (\f -> (name f) == fname) fs) in
  eval2 (body f) fs (zip (args f) (map (\a -> eval2 a fs bs) fargs))

math f (VNum l) (VNum r) = VNum (f l r) 
math _ _ _ = (error "cant do math on records")

run e fs = (eval2 (parse (sread e)) (map (\f -> (parseDef (sread f))) fs) [])

--------------------
------ tests -------
--------------------

parseTest s expected = makeTest s (parse (sread s)) expected 
parseDefTest s expected = makeTest s (parseDef (sread s)) expected 
evalSimpleTest s expected = makeTest s (eval (parse (sread s))) expected 
evalTest e fs expected = 
  makeTest e (eval2 (parse (sread e)) (map (\f -> (parseDef (sread f))) fs) []) expected 

results = runTests
 [
   parseTest "(+ 5 6)" (Add (Num 5) (Num 6)),
   parseTest "(- 5 6)" (Sub (Num 5) (Num 6)),
   parseDefTest "(deffun (f x y) (+ x y))" (FunDef "f" ["x","y"] (Add (Id "x") (Id "y"))),
   parseDefTest "(deffun (f) 7)" (FunDef "f" [] (Num 7)),
   evalSimpleTest "5" (VNum 5),
   evalSimpleTest "(+ 5 6)" (VNum 11),
   evalSimpleTest "(with (x 7) (+ x 9))" (VNum 16),
   evalSimpleTest "(with (x 7) (with (x 8) x))" (VNum 8),
   evalSimpleTest "(get (rec (c 5) (x 8) (y (rec (z 9)))) x)" (VNum 8),
   evalSimpleTest "(get (rec (c 5) (x 8) (y (rec (z 9)))) y)" (VRec [("z",VNum 9)]),
   evalSimpleTest "(get (get (rec (c 5) (x 8) (y (rec (z 9)))) y) z)" (VNum 9),
   evalTest "(f 7)" ["(deffun (f x) (+ x x))"] (VNum 14)
 ]

-- *Main Data.List Data.Maybe> (eval (parse (sread "(rec (c 5) (x 8) (y (rec (x 9))) )")))
-- *** Exception: bad rec: 
-- *Main Data.List Data.Maybe> (eval (parse (sread "(rec (c 5) (x 8) (y (rec (x 9))))")))
-- VRec [("c",VNum 5),("x",VNum 8),("y",VRec [("x",VNum 9)])]

-- (test (interp (parse '{f 1 2})
--                (list (parse-defn '{deffun {f x y} {+ x y}})))
--        3)
--  (test (interp (parse '{+ {f} {f}})
--                (list (parse-defn '{deffun {f} 5})))
--        10)
--  (test/exn (interp (parse '{f 1})
--                    (list (parse-defn '{deffun {f x y} {+ x y}})))
--            "wrong arity")

