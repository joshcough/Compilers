module L2AST where

import TestHelpers
import Test.HUnit
import L1L2AST
import L1L2Parser
import Read

-- L2 AST (uses shared L1/L2 AST)
-- L2 adds variables to X and S. that's the only difference between L2 and L1.
type Variable = String
data L2X = RegL2X Register | VariableL2X Variable
data L2S = NumberL2S Int | LabelL2S Label | RegL2S Register | VariableL2S Variable
type L2Instruction = Instruction L2X L2S
type L2Func = Func L2X L2S
type L2 = Program L2X L2S

instance Show L2X where
  show (RegL2X r)      = show r
  show (VariableL2X v) = v

instance Show L2S where
  show (NumberL2S n)   = show n
  show (LabelL2S l)    = show l
  show (RegL2S r)      = show r
  show (VariableL2S v) = v

-- L2 Parser (uses shared L1/L2 Parser)
parseL2 = parse (parseI (parseX VariableL2X RegL2X) parseL2S) where
  parseX v r  s = Right $ maybe (v $ drop 1 s) r (parseRegister s)
  parseL2S    s = case (sread s) of
    AtomNum n -> Right $ NumberL2S n
    AtomSym s -> maybe (parseX VariableL2S RegL2S s) Right $ parseLabelOrRegister LabelL2S RegL2S s
