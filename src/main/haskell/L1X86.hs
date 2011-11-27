module L1X86 where

import L1AST
import Data.List
import Monad

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

dump :: [X86Inst] -> String
dump insts = concat $ intersperse "\n" $ (map adjust insts) where 
  adjust i = if (last i == ':' || take 6 i == ".globl") then i else '\t' : i 

generateCode :: L1 -> String
generateCode (L1 main funcs) =
  dump (header ++ (generateMain main) ++ (funcs >>= generateFunc) ++ footer)

generateMain (L1Func insts) = ((tail insts) >>= genInst) ++ [
  "popl %ebp",
  "popl %edi",
  "popl %esi",
  "popl %ebx",
  "leave",
  "ret"] 


generateFunc (L1Func insts) = insts >>= genInst

genInst i = []
