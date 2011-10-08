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

type L1X = Register
data L1S = NumberL1S Int | LabelL1S Label | RegL1S Register
type L1Instruction = Instruction L1X L1S
data L1Func = L1Func [L1Instruction]
data L1 = L1 L1Func [L1Func]
