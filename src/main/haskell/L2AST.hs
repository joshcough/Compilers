module L2AST where

import TestHelpers
import Test.HUnit
import L1AST

type Variable = String
data L2X = RegL2X Register | VariableL2X Variable
data L2S = NumberL2S Int | LabelL2S Label | RegL2S Register | VariableL2S Variable
type L2Instruction = Instruction L2X L2S
data L2Func = L2Func [L2Instruction]
data L2 = L2 L2Func [L2Func]

instance Show L2X where
  show (RegL2X r) = show r
  show (VariableL2X v) = v

instance Show L2S where
  show (NumberL2S n) = show n
  show (LabelL2S l) = show l
  show (RegL2S r) = show r
  show (VariableL2S v) = v

instance Show L2 where
  show (L2 main fs) = unlines ["(", show main, fs >>= show, ")"]

instance Show L2Func where
  show (L2Func is) = "(" ++ (is >>= (\i -> ((show i) ++ "\n\t"))) ++ ")"
