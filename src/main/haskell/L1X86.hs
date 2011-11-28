module L1X86 where

import L1AST
import Data.List

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
generateCode (L1 main funcs) =
  let (x86Main, labelCount) = generateMain main in
  let (x86Funcs, _) = generateFunc ((concat (map body funcs)), labelCount) in
  dump (header ++ x86Main ++ x86Funcs ++ footer)

generateMain :: L1Func -> ([X86Inst], Int)
generateMain (L1Func insts) = 
  let (mainBody, lc) = generateFunc ((tail insts), 0) in
  (mainBody ++ mainFooter, lc)

--foldl :: (a -> b -> a) -> a -> [b] -> a
generateFunc :: ([L1Instruction], Int) -> ([X86Inst], Int)
generateFunc (insts, i) = foldl genHelper ([],i) insts where
  genHelper :: ([X86Inst], Int) -> L1Instruction -> ([X86Inst], Int)
  genHelper (acc, lc) inst = let (nextInsts, nextLabel) = genInst (inst, lc) in (acc ++ nextInsts, nextLabel)


declare label = "L1_" ++ label ++ ":"
triple op s1 s2 = op ++ " " ++ s1 ++ ", " ++ s2
genReg :: Register -> String 
genReg (CXR cx) = "%" ++ (show cx)
genReg (XR x)   = "%" ++ (show x)
genS :: L1S -> String
genS (NumberL1S i) = "$" ++ (show i)
genS (LabelL1S  l) = "$L1_" ++ l
genS (RegL1S    r) = genReg r

genInst :: (L1Instruction, Int) -> ([X86Inst], Int) 
genInst (LabelDeclaration label, i) = ([declare label], i)
genInst (Assign r@(CXR cx) (SRHS s), i) = ([triple "movl" (genS s) (genReg r)], i)
genInst (Assign r@(XR   x) (SRHS s), i) = ([triple "movl" (genS s) (genReg r)], i)
genInst (inst, i) = ([], i)


