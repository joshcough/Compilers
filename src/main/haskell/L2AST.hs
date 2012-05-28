module L2AST where

import TestHelpers
import Test.HUnit
import L1L2AST
import L1L2Parser
import Read

-- L2 AST (uses shared L1/L2 AST)
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

-- L2 Parser (uses shared L1/L2 Parser)
parseL2 s = parse (parseI (parseX VariableL2X RegL2X) parseL2S) s where
  parseX v r  s = maybe (v $ drop 1 s) r (parseRegister s)
  parseL2S    s = case (sread s) of
    AtomNum n -> NumberL2S n
    AtomSym s -> maybe ((parseX VariableL2S RegL2S) s) toL2S (parseLabelOrRegister s)
  toL2S (LL l) = LabelL2S l
  toL2S (LR r) = RegL2S   r
