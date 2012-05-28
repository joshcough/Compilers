module L1L2Parser where

import Read
import TestHelpers
import Test.HUnit
import L1L2AST
import Data.Maybe

parse :: (SExpr -> Instruction x s) -> SExpr -> Program x s
parse parseInst (List ((List main) : funcs)) =
  Program (parseMain parseInst main) (map (parseFunction parseInst) funcs) where
parse _ bad = error $ "bad program" ++ show bad

parseMain :: (SExpr -> Instruction x s) -> [SExpr] -> Func x s
parseMain parseInst exps = Func $ (LabelDeclaration "main") : (map parseInst exps)

parseFunction :: (SExpr -> Instruction x s) -> SExpr -> Func x s
parseFunction parseInst (List ((AtomSym name) : exps)) =
  Func $ (LabelDeclaration (parseLabel name)) : (map parseInst exps)

parseI :: (String -> x) -> (String -> s) -> SExpr -> Instruction x s
parseI _ _ a@(AtomSym s) =
  maybe (error $ "not an instruction " ++ show a) getLabel (parseLabelOrRegister s) where
  getLabel (LL l) = LabelDeclaration l
  getLabel (LR r) =  error $ "expected a label " ++ show a
parseI _ _ a@(AtomNum n) = error $ "bad instruction" ++ show a
parseI parseX parseS l@(List ss) = case (flatten l) of
  [x, "<-", "print", t] -> Assign (parseX x) (Print (parseT t))
  [x, "<-", "allocate", t1, t2] -> Assign (parseX x) (Allocate (parseT t1) (parseT t2))
  [x, "<-", "array-error", t1, t2] -> Assign (parseX x) (ArrayError (parseT t1) (parseT t2))
  [x, "<-", s]  -> Assign (parseX x) (SRHS (parseS s))
  [x1, "<-", "mem", x2, n4] -> Assign (parseX x1) (MemRead (MemLoc (parseX x2) (parseN4 n4)))
  ["mem", x, n4, "<-", s] -> MemWrite (MemLoc (parseX x) (parseN4 n4)) (parseS s)
  [x, "+=", t]  -> Increment  (parseX x) (parseT t)
  [x, "-=", t]  -> Decrement  (parseX x) (parseT t)
  [x, "*=", t]  -> Multiply   (parseX x) (parseT t)
  [x, "&=", t]  -> BitwiseAnd (parseX x) (parseT t)
  [x, ">>=", t] -> RightShift (parseX x) (parseT t)
  [x, "<<=", t] -> LeftShift  (parseX x) (parseT t)
  [cx, "<-", t1, cmp, t2] -> Assign (parseX cx) (CompRHS (parseComp t1 cmp t2))
  ["goto", l] -> Goto (parseLabel l)
  ["cjump", t1, cmp, t2, l1, l2] -> CJump (parseComp t1 cmp t2) (parseLabel l1) (parseLabel l2)
  ["call", u]      -> Call (parseU u)
  ["tail-call", u] -> TailCall (parseU u)
  ["return"]       -> Return
  {-
  I wanted these at the bottom, but had to move them to the top
  because of some weird compiler error.
  parseB :: [String] -> String
  parseB ["eax", "<-", "allocate", t1, t2] = "allocate"
  parseB ["eax", "<-", "array-error", t1, t2] = "array-error"
  -}
  where
    parseT   = parseS
    parseU   = parseS

    parseComp t1 cmp t2 = Comp (parseT t1) (compOpFromSym cmp) (parseT t2)

parseLabel :: String -> Label
parseLabel s = drop 1 s

data LabelOrReg = LL Label | LR Register

parseLabelOrRegister :: String -> Maybe LabelOrReg
parseLabelOrRegister l@(':' : _) = Just $ LL $ parseLabel l
parseLabelOrRegister r           = fmap LR $ parseRegister r

parseRegister :: String -> Maybe Register
parseRegister r = registerFromName r
parseN4 n = case (sread n) of
  AtomNum n -> n
  AtomSym s -> error $ "not a number" ++ n
parseCXRegister cx = fmap CXR (cxRegisterFromName cx)
