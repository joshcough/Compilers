module L2AST where

import TestHelpers
import Test.HUnit
import L1AST

data Variable = Variable String
data L2X = RegL2X Register | VariableL2X Variable
data L2S = NumberL12 Int | LabelL2S Label | RegL2S Register | VariableL2S Variable
type L2Instruction = Instruction L2X L2S
data L2Func = L2Func L2Instruction
data L2 = L2 L2Func [L2Func]
