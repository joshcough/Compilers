module L1L2AST where

import TestHelpers
import Test.HUnit

data XRegister  = Esi | Edi | Ebp | Esp
data CXRegister = Eax | Ebx | Ecx | Edx
data Register   = CXR CXRegister | XR XRegister

type Label    = String
data MemLoc x = MemLoc x Int
data CompOp   = LT | LTEQ | EQ
data Comp s   = Comp s CompOp s

data AssignRHS x s = 
  CompRHS (Comp s)   | 
  Allocate s s       |
  Print s            |
  ArrayError s s     |
  MemRead (MemLoc x) |
  SRHS s

data Instruction x s = 
  Assign x (AssignRHS x s)   |
  Increment x s              |
  Decrement x s              |
  Multiply x s               |
  LeftShift x s              |
  RightShift x s             |
  BitwiseAnd x s             |
  MemWrite (MemLoc x) s      |
  Goto Label                 |
  CJump (Comp s) Label Label |
  LabelDeclaration Label     |
  Call s                     |
  TailCall s                 |
  Return 

data Func x s = Func { body :: [Instruction x s]}
data Program x s = Program (Func x s) [Func x s]

instance (Show x, Show s) => Show (Program x s) where
  show (Program main fs) = unlines ["(", show main, fs >>= show, ")"]

instance (Show x, Show s) => Show (Func x s) where
  show (Func is) = "(" ++ (is >>= (\i -> ((show i) ++ "\n\t"))) ++ ")"

instance (Show x, Show s) => Show (Instruction x s) where
  show (Assign x rhs)       = "(" ++ (show x) ++ " <- "   ++ (show rhs) ++ ")"
  show (Increment x s)      = "(" ++ (show x) ++ " += "   ++ (show s) ++ ")"
  show (Decrement x s)      = "(" ++ (show x) ++ " -= "   ++ (show s) ++ ")"
  show (Multiply x s)       = "(" ++ (show x) ++ " *= "   ++ (show s) ++ ")"
  show (LeftShift x s)      = "(" ++ (show x) ++ " <<= "  ++ (show s) ++ ")"
  show (RightShift x s)     = "(" ++ (show x) ++ " >>= "  ++ (show s) ++ ")"
  show (BitwiseAnd x s)     = "(" ++ (show x) ++ " &= "   ++ (show s) ++ ")"
  show (MemWrite loc s)     = "(" ++ (show loc) ++ " <- " ++ (show s) ++ ")"
  show (Goto l)             = "(goto" ++ (show l) ++ ")"
  show (CJump cmp l1 l2)    = concat ["(", (show cmp), " ", (show l1), " ", (show l2), ")"]
  show (LabelDeclaration l) = show l
  show (Call s)             = "(call" ++ (show s) ++ ")"
  show (TailCall s)         = "(tail-call" ++ (show s) ++ ")"
  show Return               = "(return)"

instance Show XRegister where
  show Esi = "esi"
  show Edi = "edi"
  show Ebp = "ebp"
  show Esp = "esp"
instance Show CXRegister where
  show Eax = "eax"
  show Ebx = "ebx"
  show Ecx = "ecx"
  show Edx = "edx"

instance Show Register where
  show (CXR cxr) = show cxr
  show (XR xr)   = show xr

instance (Show x, Show s) => Show (AssignRHS x s) where
  show (CompRHS c)        = show c
  show (Allocate s1 s2)   = "(allocate " ++ (show s1) ++ " " ++ (show s2) ++ ")"
  show (Print s)          = "(print " ++ (show s) ++ ")"
  show (ArrayError s1 s2) = "(array-error " ++ (show s1) ++ " " ++ (show s2) ++ ")"
  show (SRHS s)           = show s
  show (MemRead loc)      = show loc

instance (Show x) => Show (MemLoc x) where
  show (MemLoc x n) = "(mem " ++ (show x) ++ " " ++ (show n) ++ ")"

instance (Show s) => Show (Comp s) where
  show (Comp s1 op s2) = (show s1) ++ " " ++ (show op) ++ " " ++ (show s2)

xRegisters      = [Esi, Edi, Ebp, Esp]
cxRegisters     = [Eax, Ebx, Ecx, Edx]
xRegisterNames  = map show xRegisters
cxRegisterNames = map show cxRegisters

xRegisterFromName :: String -> Maybe XRegister
xRegisterFromName "esi" = Just Esi
xRegisterFromName "edi" = Just Edi
xRegisterFromName "ebp" = Just Ebp
xRegisterFromName "esp" = Just Esp
xRegisterFromName _     = Nothing

cxRegisterFromName :: String -> Maybe CXRegister
cxRegisterFromName "eax" = Just Eax
cxRegisterFromName "ebx" = Just Ebx
cxRegisterFromName "ecx" = Just Ecx
cxRegisterFromName "edx" = Just Edx
cxRegisterFromName _     = Nothing

registerFromName :: String -> Maybe Register
registerFromName s = maybe (fmap CXR (cxRegisterFromName s)) (Just . XR) (xRegisterFromName s)

instance Show CompOp where
  show L1L2AST.LT   = "<"
  show L1L2AST.LTEQ = "<="
  show L1L2AST.EQ   = "="

compOpFromSym :: String -> CompOp
compOpFromSym "<"  = L1L2AST.LT
compOpFromSym "<=" = L1L2AST.LTEQ
compOpFromSym "="  = L1L2AST.EQ
compOpFromSym s    = error $ "not a comparison operator" ++ s

runOp L1L2AST.LT   n1 n2 = n1 <  n2
runOp L1L2AST.LTEQ n1 n2 = n1 <= n2
runOp L1L2AST.EQ   n1 n2 = n1 == n2

foldOp :: a -> a -> a -> CompOp -> a
foldOp a _ _ L1L2AST.LT   = a
foldOp _ a _ L1L2AST.LTEQ = a
foldOp _ _ a L1L2AST.EQ   = a