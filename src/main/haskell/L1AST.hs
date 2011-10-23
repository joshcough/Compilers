module L1AST where

import TestHelpers
import Test.HUnit
import Maybe

data XRegister = Esi | Edi | Ebp | Esp
data CXRegister = Eax | Ebx | Ecx | Edx
data Register = CXR CXRegister | XR XRegister

instance Show XRegister where
  show Esi = "esi"
  show Edi = "edi"
  show Ebp = "ebp"
  show Esp = "esp"
instance Show CXRegister where
  show Eax = "esi"
  show Ebx = "ebx"
  show Ecx = "ecx"
  show Edx = "edx"

xRegisters = [Esi, Edi, Ebp, Esp]
cxRegisters = [Eax, Ebx, Ecx, Edx]
xRegisterNames = map show xRegisters
cxRegisterNames = map show cxRegisters

xRegisterFromName :: String -> Maybe XRegister
xRegisterFromName "esi" = Just Esi
xRegisterFromName "edi" = Just Edi
xRegisterFromName "ebp" = Just Ebp
xRegisterFromName "esp" = Just Esp
xRegisterFromName _ = Nothing

cxRegisterFromName :: String -> Maybe CXRegister
cxRegisterFromName "eax" = Just Eax
cxRegisterFromName "ebx" = Just Ebx
cxRegisterFromName "ecx" = Just Ecx
cxRegisterFromName "edx" = Just Edx
cxRegisterFromName _ = Nothing

registerFromName :: String -> Maybe Register
registerFromName name = case (xRegisterFromName name) of
  Just xr -> Just (XR xr)
  Nothing -> fmap CXR (cxRegisterFromName name)

type Label = String

data MemLoc x = MemLoc x Int

data CompOp = LT | LTEQ | EQ

instance Show CompOp where
  show L1AST.LT = "<"
  show LTEQ = "<="
  show L1AST.EQ = "="

compOpFromSym :: String -> CompOp
compOpFromSym "<"  = L1AST.LT
compOpFromSym "<=" = LTEQ
compOpFromSym "="  = L1AST.EQ
compOrFromSym _ = error "not a comparison operator"

data Comp s = Comp s CompOp s
data AssignmentRHS x s = 
  CompRHS (Comp s) | 
  Allocate s s |
  Print s |
  ArrayError s s |
  MemRead (MemLoc x) |
  SRHS s

data Instruction x s = 
  Assignment x (AssignmentRHS x s) |
  Increment x s |
  Decrement x s |
  Multiply x s |
  LeftShift x s |
  RightShift x s |
  BitwiseAnd x s |
  MemWrite (MemLoc x) s |
  Goto Label |
  CJump (Comp s) Label Label |
  LabelDeclaration Label |
  Call s |
  TailCall s |
  Return 

type L1X = Register
data L1S = NumberL1S Int | LabelL1S Label | RegL1S Register
type L1Instruction = Instruction L1X L1S
data L1Func = L1Func [L1Instruction]
data L1 = L1 L1Func [L1Func]
