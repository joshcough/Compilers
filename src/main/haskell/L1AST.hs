module L1AST where

import TestHelpers
import Test.HUnit

data XRegister = Esi | Edi | Ebp | Esp
data CXRegister = Eax | Ebx | Ecx | Edx
data Register = CXR CXRegister | XR XRegister
type Label = String

data MemLoc x = MemLoc x Int
data CompOp = LT | LTEQ | EQ
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

data Func x s = Func [(Instruction x s)]
data Lang x s = Lang (Func x s) [Func x s]

type L1X = Register
data L1S = NumberL1S Int | LabelL1S Label | RegL1S Register
type L1 = Lang L1X L1S
