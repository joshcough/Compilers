module L2 where

import TestHelpers
import Test.HUnit
import L1AST

data Variable = Variable String
data L2X = RegL2X Register | VariableL2X Variable
data L2S = NumberL12 Int | LabelL2S Label | RegL2S Register | VariableL2S Variable
type L2 = Lang L2X L2S

