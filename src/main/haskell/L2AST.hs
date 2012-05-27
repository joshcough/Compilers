module L2AST where

import TestHelpers
import Test.HUnit
import L1AST

type Variable = String
data L2X = RegL2X Register | VariableL2X Variable -- type L1X = Register
data L2S = NumberL2S Int | LabelL2S Label | RegL2S Register | VariableL2S Variable -- data L1S = NumberL1S Int | LabelL1S Label | RegL1S Register
type L2Instruction = Instruction L2X L2S
data L2Func = Func L2X L2S
data L2 = Program L2X L2S

instance Show L2X where
  show (RegL2X r) = show r
  show (VariableL2X v) = v

instance Show L2S where
  show (NumberL2S n) = show n
  show (LabelL2S l) = show l
  show (RegL2S r) = show r
  show (VariableL2S v) = v

