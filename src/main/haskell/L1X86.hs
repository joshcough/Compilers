module L1X86 where

import L1AST

type X86Inst = String

header = [
  ".file \"prog.c\"",
  ".text",
  ".globl go",
  ".type  go, @function",
  "go:",
  "pushl %ebp",
  "movl %esp, %ebp",
  "pushl %ebx",
  "pushl %esi",
  "pushl %edi",
  "pushl %ebp",
  "movl  %esp, %ebp"]

generateCode :: L1 -> String
generateCode _ = []
