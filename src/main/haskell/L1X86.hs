module L1X86 where

import L1AST
import Data.List
import Data.Traversable
import Control.Monad.State

type X86Inst = String

header = [
  ".file \"prog.c\"",
  ".text",
  ".globl go",
  ".type  go, @function",
  "go:",
  "pushl %ebp",
  "movl  %esp, %ebp",
  "pushl %ebx",
  "pushl %esi",
  "pushl %edi",
  "pushl %ebp",
  "movl  %esp, %ebp" ]

footer = [
  ".size   go, .-go",
  ".ident  \"GCC: (Ubuntu 4.3.2-1ubuntu12) 4.3.2\"",
  ".section .note.GNU-stack,\"\",@progbits" ]

genCode :: L1 -> String
genCode l1 = fst (runState (genCodeS l1) 0)

genCodeS :: L1 -> State Int String
genCodeS (L1 main funcs) =
  do x86Main  <- genMain main -- todo: drop first from main here?
     x86Funcs <- genFunc $ concat $ map body funcs
     return $ dump $ join [header, x86Main, x86Funcs, footer] where
  dump :: [X86Inst] -> String
  dump insts = concat $ intersperse "\n" $ (map adjust insts) where
    adjust i = if (last i == ':' || take 6 i == ".globl") then i else '\t' : i

genMain :: L1Func -> State Int [X86Inst]
genMain (L1Func insts) = 
  (flip fmap) (genFunc (tail insts)) (++ mainFooter) where
  mainFooter = [
    "popl %ebp",
    "popl %edi",
    "popl %esi",
    "popl %ebx",
    "leave",
    "ret" ] 

genFunc :: [L1Instruction] -> State Int [X86Inst] 
genFunc insts = (traverse genInstS insts) >>= return . join

declare label = "L1_" ++ label ++ ":"
triple op s1 s2 = op ++ " " ++ s1 ++ ", " ++ s2
genReg :: Register -> String 
genReg (CXR cx) = "%" ++ (show cx)
genReg (XR x)   = "%" ++ (show x)
genS :: L1S -> String
genS (NumberL1S i) = "$" ++ (show i)
genS (LabelL1S  l) = "$L1_" ++ l
genS (RegL1S    r) = genReg r
--genS (RegL1S    r) = "%" ++ (show r)
genLoc (MemLoc r i) = join [(show i), "(", genReg r, ")"] 

jump :: L1S -> String
jump (LabelL1S name) = "jmp L1_" ++ name
jump l               = "jmp *" ++ (genS l)

low8 cx = "%" ++ [((show cx) !! 1)] ++ "l"
setInstruction L1AST.LT   = "setl"
setInstruction L1AST.LTEQ = "setle"
setInstruction L1AST.EQ   = "sete"

jumpIfLess            l = "jl L1_"  ++ (show l)
jumpIfLessThanOrEqual l = "jle L1_" ++ (show l) 
jumpIfGreater         l = "jg L1_"  ++ (show l) 
jumpIfGreaterOrEqual  l = "jge L1_" ++ (show l) 
jumpIfEqual           l = "je L1_"  ++ (show l) 

postIncrement = do { x <- get; put (x+1); return x }

genInstS :: L1Instruction -> State Int [X86Inst]
genInstS (Call s) =
  do i <- postIncrement
     let label = "Generated_Label_" ++ (show i)
     return [
       "pushl " ++ (genS (LabelL1S label)),
       "pushl %ebp",
       "movl %esp, %ebp",
       jump s,
       declare label ]
genInstS i = return $ genInst i

genInst :: L1Instruction -> [X86Inst]
genInst (LabelDeclaration label)     = [declare label]
genInst (Assign l r)       = genAssignInst l r
genInst (MemWrite   loc s) = [triple "movl"  (genS s) (genLoc loc)]
genInst (Increment  r s)   = [triple "addl"  (genS s) (genReg r)]
genInst (Decrement  r s)   = [triple "subl"  (genS s) (genReg r)]
genInst (Multiply   r s)   = [triple "imull" (genS s) (genReg r)]
genInst (RightShift r s)   = [triple "sarl"  (genS s) (genReg r)]
genInst (LeftShift  r s)   = [triple "sall"  (genS s) (genReg r)]
genInst (BitwiseAnd r s)   = [triple "andl"  (genS s) (genReg r)]
genInst (Goto s)           = [jump (LabelL1S s)]
genInst (TailCall s)       = ["movl %ebp, %esp", jump s]
-- special case for two numbers
genInst (CJump (Comp l@(NumberL1S n1) op r@(NumberL1S n2)) l1 l2) = 
  if (runOp op n1 n2) then [jump (LabelL1S l1)] else [jump (LabelL1S l2)]
-- (cjump 11 < ebx :true :false) special case. destination must be a register.
genInst (CJump (Comp l@(NumberL1S n) op r@(RegL1S _)) l1 l2) = [
  triple "cmpl" (genS l) (genS r),
  jumpInstruction op,
  jump (LabelL1S l2) ] where
  jumpInstruction L1AST.LT   = jumpIfGreater l1
  jumpInstruction L1AST.LTEQ = jumpIfGreaterOrEqual l1
  jumpInstruction L1AST.EQ   = jumpIfEqual l1
genInst (CJump (Comp s1 op s2) l1 l2) = [
  triple "cmpl" (genS s2) (genS s1),
  jumpInstruction op,
  jump (LabelL1S l2) ] where
  jumpInstruction L1AST.LT   = jumpIfLess l1
  jumpInstruction L1AST.LTEQ = jumpIfLessThanOrEqual l1
  jumpInstruction L1AST.EQ   = jumpIfEqual l1
genInst Return = [
  "movl %ebp, %esp",
  "popl %ebp",
  "ret" ]
genInst _ = return []

-- several assignment cases
genAssignInst r (SRHS s)          = [triple "movl" (genS s) (genReg r)]
genAssignInst r (MemRead loc)     = [triple "movl" (genLoc loc) (genReg r)]
{- 
cmp assignments have to be with CXRegisters on LHS
(eax <- ebx < ecx)
Here we need another trick; the x86 instruction set only let us
update the lowest 8 bits with the result of a condition code. So,
we do that, and then fill out the rest of the bits with zeros with
a separate instruction:

  cmp %ecx, %ebx
  setl %al
  movzbl %al, %eax
-}
genAssignInst cx@(CXR c) (CompRHS (Comp l@(RegL1S _) op r@(RegL1S _))) = 
  genCompInst cx r op l setInstruction
genAssignInst cx@(CXR c) (CompRHS (Comp l@(NumberL1S _) op r@(RegL1S _))) =  
  -- magic reverse happens here!
  genCompInst cx l op r reverseCmpInstruction where
  reverseCmpInstruction L1AST.LT   = "setg"
  reverseCmpInstruction L1AST.LTEQ = "setge"
  reverseCmpInstruction L1AST.EQ   = "sete"
genAssignInst cx@(CXR c) (CompRHS (Comp l@(RegL1S _) op r@(NumberL1S _))) =
  genCompInst cx r op l setInstruction
genAssignInst cx@(CXR _) (CompRHS (Comp l@(NumberL1S n1) op r@(NumberL1S n2))) = 
  [triple "movl" ("$" ++ (if (runOp op n1 n2) then "1" else "0")) (genReg cx)]
genAssignInst (CXR Eax) (Print s) = [
  "pushl " ++ (genS s),
  "call print",
  "addl $4, %esp" ]
genAssignInst (CXR Eax) (Allocate s n) = [
  "pushl " ++ (genS n),
  "pushl " ++ (genS s),
  "call allocate",
  "addl $8, %esp" ]
genAssignInst (CXR Eax) (ArrayError s n) = [
  "pushl " ++ (genS n),
  "pushl " ++ (genS s),
  "call print_error",
  "addl $8, %esp" ]
-- todo: should i bother changing the return type to Either[String, [X86Inst]]?
genAssignInst l r = error ("bad assignment statement: " ++ (show (Assign l r)))

genCompInst cx@(CXR c) l op r f = [
  triple "cmp" (genS l) (genS r),
  (f op) ++ " " ++ (low8 c),
  triple "movzbl" (low8 c) (genReg cx) ]

