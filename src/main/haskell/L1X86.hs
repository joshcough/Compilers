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
  "movl  %esp, %ebp"]

footer = [
  ".size   go, .-go",
  ".ident  \"GCC: (Ubuntu 4.3.2-1ubuntu12) 4.3.2\"",
  ".section .note.GNU-stack,\"\",@progbits"]

mainFooter = [
  "popl %ebp",
  "popl %edi",
  "popl %esi",
  "popl %ebx",
  "leave",
  "ret"] 

dump :: [X86Inst] -> String
dump insts = concat $ intersperse "\n" $ (map adjust insts) where 
  adjust i = if (last i == ':' || take 6 i == ".globl") then i else '\t' : i 

generateCode :: L1 -> String
generateCode l1 = fst (runState (genCodeS l1) 0)

genCodeS :: L1 -> State Int String
genCodeS (L1 main funcs) =
  do
    x86Main  <- generateMain main -- todo: drop first from main here?
    x86Funcs <- generateFunc $ concat $ map body funcs
    return $ dump $ join [header, x86Main, x86Funcs, footer]

generateMain :: L1Func -> State Int [X86Inst]
generateMain (L1Func insts) = 
  (flip fmap) (generateFunc (tail insts)) (++ mainFooter)

generateFunc :: [L1Instruction] -> State Int [X86Inst] 
generateFunc insts = (traverse genInstS insts) >>= return . join

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

jump :: L1S -> String
jump (LabelL1S name) = "jmp L1_" ++ name
jump l               = "jmp *" ++ (genS l)

genInstS :: L1Instruction -> State Int [X86Inst]
genInstS (Call s) =
  do
    i <- get
    _ <- put (i+1)
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
genInst (Assign r@(CXR cx) (SRHS s)) = [triple "movl" (genS s) (genReg r)]
genInst (Assign r@(XR   x) (SRHS s)) = [triple "movl" (genS s) (genReg r)]
genInst inst = return []
