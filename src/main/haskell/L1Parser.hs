module L1Parser where

import Read
import TestHelpers
import Test.HUnit
import L1AST
import Data.Maybe

parse :: SExpr -> L1
parse (List ((List main) : funcs)) = L1 (parseMain main) (map parseFunction funcs)
parse bad = error $ "bad L1 program" ++ show bad

parseMain :: [SExpr] -> L1Func
parseMain exps = L1Func $ (LabelDeclaration "main") : (map parseI exps)

parseFunction :: SExpr -> L1Func
parseFunction (List ((AtomSym name) : exps)) = 
  L1Func $ (LabelDeclaration (parseLabel name)) : (map parseI exps)

parseI :: SExpr -> L1Instruction
parseI a@(AtomSym s) =
  either LabelDeclaration (const $ error $ "not an instruction " ++ show a) (parseLabelOrRegister s)
parseI a@(AtomNum n) = error $ "bad instruction" ++ show a
parseI l@(List ss) = case (flatten l) of
  ["eax", "<-", "print", t] -> Assign (CXR Eax) (Print (parseT t))
  ["eax", "<-", "allocate", t1, t2] -> Assign (CXR Eax) (Allocate (parseT t1) (parseT t2))
  ["eax", "<-", "array-error", t1, t2] -> Assign (CXR Eax) (ArrayError (parseT t1) (parseT t2))
  [x, "<-", s]  -> Assign (parseX x) (SRHS (parseS s))
  [x1, "<-", "mem", x2, n4] -> Assign (parseX x1) (MemRead (MemLoc (parseX x2) (parseN4 n4)))
  ["mem", x, n4, "<-", s] -> MemWrite (MemLoc (parseX x) (parseN4 n4)) (parseS s)
  [x, "+=", t]  -> Increment  (parseX x) (parseT t)
  [x, "-=", t]  -> Decrement  (parseX x) (parseT t)
  [x, "*=", t]  -> Multiply   (parseX x) (parseT t)
  [x, "&=", t]  -> BitwiseAnd (parseX x) (parseT t)
  [x, ">>=", t] -> RightShift (parseX x) (parseT t)
  [x, "<<=", t] -> LeftShift  (parseX x) (parseT t)
  [cx, "<-", t1, cmp, t2] -> Assign (parseCXRegister cx) (CompRHS (parseComp t1 cmp t2))
  ["goto", l] -> Goto (parseLabel l)
  ["cjump", t1, cmp, t2, l1, l2] -> CJump (parseComp t1 cmp t2) (parseLabel l1) (parseLabel l2)
  ["call", u]      -> Call (parseU u)
  ["tail-call", u] -> TailCall (parseU u)
  ["return"]       -> Return
{-
parseB :: [String] -> String
parseB ["eax", "<-", "allocate", t1, t2] = "allocate"
parseB ["eax", "<-", "array-error", t1, t2] = "array-error"
-}

parseLabel :: String -> Label
parseLabel s = drop 1 s

parseS :: String -> L1S
parseS s = case (sread s) of
  AtomNum n -> NumberL1S n
  AtomSym s -> either LabelL1S RegL1S (parseLabelOrRegister s) 

parseLabelOrRegister :: String -> Either Label Register
parseLabelOrRegister l@(':' : _) = Left  (parseLabel l)
parseLabelOrRegister r           = Right (parseRegister r)

parseT   = parseS
parseU   = parseS
parseX x = parseRegister x

parseRegister :: String -> Register
parseRegister r = fromMaybe (error ("not a register " ++ (show r))) (registerFromName r)
parseN4 n = case (sread n) of
  AtomNum n -> n
  AtomSym s -> error $ "not a number" ++ n
parseCXRegister cx = CXR (fromMaybe (error "not a register") (cxRegisterFromName cx))

parseComp t1 cmp t2 = Comp (parseT t1) (compOrFromSym cmp) (parseT t2)
