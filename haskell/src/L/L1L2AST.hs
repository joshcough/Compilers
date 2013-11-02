{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module L.L1L2AST where

type Label      = String

data XRegister  = Esi | Edi | Ebp | Esp deriving Eq
data CXRegister = Eax | Ebx | Ecx | Edx deriving Eq
data Register   = CXR CXRegister | XR XRegister  deriving Eq
esi = XR Esi
edi = XR Edi
ebp = XR Ebp
esp = XR Esp
eax = CXR Eax
ebx = CXR Ebx
ecx = CXR Ecx
edx = CXR Edx
data MemLoc x   = MemLoc x Int
data CompOp     = LT | LTEQ | EQ
data Comp s     = Comp s CompOp s
data AssignRHS x s =
  CompRHS (Comp s) | Allocate s s | Print s | ArrayError s s | MemRead (MemLoc x) | SRHS s

data X86Op = Increment | Decrement | Multiply | LeftShift | RightShift | BitwiseAnd
  deriving (Eq)
increment  = Increment
decrement  = Decrement
multiply   = Multiply
leftShift  = LeftShift
rightShift = RightShift
bitwiseAnd = BitwiseAnd

x86OpSymbol Increment  = "+="
x86OpSymbol Decrement  = "-="
x86OpSymbol Multiply   = "*="
x86OpSymbol LeftShift  = "<<="
x86OpSymbol RightShift = ">>="
x86OpSymbol BitwiseAnd = "&="

x86OpName Increment  = "addl"
x86OpName Decrement  = "subl"
x86OpName Multiply   = "imull"
x86OpName LeftShift  = "sall"
x86OpName RightShift = "sarl"
x86OpName BitwiseAnd = "andl"

data Instruction x s =
  Assign x (AssignRHS x s)   |
  MathInst x X86Op s         |
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
  show (Assign x rhs)       = concat ["(", show x, " <- ", show rhs, ")"]
  show (MathInst x op s)    = concat ["(", show x, " ", x86OpName op, " ", show s, ")"]
  show (MemWrite loc s)     = concat ["(", show loc, " <- ", show s, ")"]
  show (Goto l)             = concat ["(goto", show l, ")"]
  show (CJump cmp l1 l2)    = concat ["(", show cmp, " ", show l1, " ", show l2, ")"]
  show (LabelDeclaration l) = show l
  show (Call s)             = "(call" ++ show s ++ ")"
  show (TailCall s)         = "(tail-call" ++ show s ++ ")"
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
  show (Comp s1 op s2) = concat [show s1, " ", show op, " ", show s2]

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
  show L.L1L2AST.LT   = "<"
  show L.L1L2AST.LTEQ = "<="
  show L.L1L2AST.EQ   = "="

compOpFromSym :: String -> Either String CompOp
compOpFromSym "<"  = Right L.L1L2AST.LT
compOpFromSym "<=" = Right L.L1L2AST.LTEQ
compOpFromSym "="  = Right L.L1L2AST.EQ
compOpFromSym s    = Left $ "not a comparison operator" ++ s

runOp L.L1L2AST.LT   n1 n2 = n1 <  n2
runOp L.L1L2AST.LTEQ n1 n2 = n1 <= n2
runOp L.L1L2AST.EQ   n1 n2 = n1 == n2

foldOp :: a -> a -> a -> CompOp -> a
foldOp a _ _ L.L1L2AST.LT   = a
foldOp _ a _ L.L1L2AST.LTEQ = a
foldOp _ _ a L.L1L2AST.EQ   = a

-- L1 AST (uses shared L1/L2 AST)
type L1X = Register
data L1S = NumberL1S Int | LabelL1S Label | RegL1S Register
type L1Instruction = Instruction L1X L1S
type L1Func = Func L1X L1S
type L1 = Program L1X L1S

instance Show L1S where
  show (NumberL1S n) = show n
  show (LabelL1S l)  = show l
  show (RegL1S r)    = show r

-- L2 AST (uses shared L1/L2 AST)
-- L2 adds variables to X and S. that's the only difference between L2 and L1.
type Variable = String
data L2X = RegL2X Register | VarL2X Variable
data L2S = XL2S L2X | NumberL2S Int | LabelL2S Label
type L2Instruction = Instruction L2X L2S
type L2Func = Func L2X L2S
type L2 = Program L2X L2S

instance Show L2X where
  show (RegL2X r) = show r
  show (VarL2X v) = v

instance Show L2S where
  show (NumberL2S n)   = show n
  show (LabelL2S l)    = show l
  show (XL2S x)        = show x

instance Eq  L2X where (==) x1 x2 = (show x1) == (show x1)
instance Ord L2X where compare x1 x2 = compare (show x1) (show x1)
instance Ord Register where compare x1 x2 = compare (show x1) (show x1)

class (Eq a, Ord a) => AsL2X a where 
  asL2X :: a -> L2X
instance AsL2X Register where 
  asL2X = RegL2X
instance AsL2X Variable where 
  asL2X = VarL2X
instance AsL2X L2X where
  asL2X = id

orderedPair :: Ord a => a -> a -> (a, a)
orderedPair a1 a2 = if (a1 < a2) then (a1, a2) else (a2, a1)
