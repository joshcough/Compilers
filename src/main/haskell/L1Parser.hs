module L1Parser where

import Read (SExpr(AtomSym, AtomNum, List), sread)
import TestHelpers
import Test.HUnit
import L1AST

parse :: SExpr -> L1
parse (List ((List main) : funcs)) = L1 (parseMain main) (map parseFunction funcs)
parse bad = error "bad L1 program"

parseMain :: [SExpr] -> L1Func
parseMain exps = L1Func ((LabelDeclaration "main") : (map parseInstruction exps))

parseFunction :: SExpr -> L1Func
parseFunction (List ((AtomSym name) : exps)) = 
  L1Func $ (LabelDeclaration (parseLabel name)) : (map parseInstruction exps)

parseLabel :: String -> Label
parseLabel s = drop 2 s

parseInstruction :: SExpr -> L1Instruction
parseInstruction _ = error "hi"

{-
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
-}
