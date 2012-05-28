import System.Environment 
import System.IO
import Data.List
import Read
import L1L2AST
import L1L2Parser
import IOHelpers
import Data.Traversable
import Control.Monad.State

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

-- L1 Parser (uses shared L1/L2 Parser)
parseL1 = parse (parseI parseL1Reg parseL1S) where
  parseL1Reg s = maybe (error $ "invalid register: " ++ s) id (parseRegister s)
  parseL1S s = case (sread s) of
    AtomNum n -> NumberL1S n
    AtomSym s -> maybe (error $ "invalid s: " ++ s) id $ parseLabelOrRegister LabelL1S RegL1S s

-- X86 Generation code
type X86Inst = String

genX86Code :: L1 -> String
genX86Code l1 = fst $ runState (genCodeS l1) 0 where
  genCodeS :: L1 -> State Int String
  genCodeS (Program main funcs) = do
    x86Main  <- genMain main
    x86Funcs <- genFunc $ concat $ map body funcs
    return $ dump $ concat [header, x86Main, x86Funcs, footer, ["\n"]] where
    dump :: [X86Inst] -> String
    dump insts = concat $ intersperse "\n" $ map adjust insts
    adjust i = if (last i == ':' || take 6 i == ".globl") then i else '\t' : i
    header = [
      ".file\t\"prog.c\"",
      ".text",
      ".globl go",
      ".type\tgo, @function",
      "go:",
      "pushl %ebp",
      "movl %esp, %ebp",
      "pushl %ebx",
      "pushl %esi",
      "pushl %edi",
      "pushl %ebp",
      "movl\t%esp, %ebp" ]
    footer = [
      ".size\tgo, .-go",
      ".ident\t\"GCC: (Ubuntu 4.3.2-1ubuntu12) 4.3.2\"",
      ".section\t.note.GNU-stack,\"\",@progbits" ]
  
  genMain :: L1Func -> State Int [X86Inst]
  genMain (Func insts) =  (flip fmap) (genFunc (tail insts)) (++ mainFooter) where
    mainFooter = [
      "popl %ebp",
      "popl %edi",
      "popl %esi",
      "popl %ebx",
      "leave",
      "ret" ]
  
  genFunc :: [L1Instruction] -> State Int [X86Inst]
  genFunc insts = (traverse genInstS insts) >>= return . concat
  
  genInstS :: L1Instruction -> State Int [X86Inst]
  genInstS (Call s) = fmap (\i -> call $ "Generated_Label_" ++ show i) postIncrement where
    postIncrement = do { x <- get; put (x+1); return x }
    call label = [
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
    if (runOp op n1 n2) then [jump $ LabelL1S l1] else [jump $ LabelL1S l2]
  -- (cjump 11 < ebx :true :false) special case. destination must be a register.
  genInst (CJump (Comp l@(NumberL1S n) op r@(RegL1S _)) l1 l2) = [
    triple "cmpl" (genS l) (genS r),
    foldOp (jumpIfGreater l1) (jumpIfGreaterOrEqual l1) (jumpIfEqual l1) op,
    jump (LabelL1S l2) ]
  genInst (CJump (Comp s1 op s2) l1 l2) = [
    triple "cmpl" (genS s2) (genS s1),
    foldOp (jumpIfLess l1) (jumpIfLessThanOrEqual l1) (jumpIfEqual l1) op,
    jump (LabelL1S l2) ]
  genInst Return = [
    "movl %ebp, %esp",
    "popl %ebp",
    "ret" ]
  genInst i = error $ "bad instruction: " ++ show i
  
  -- several assignment cases
  genAssignInst r (SRHS s)      = [triple "movl" (genS s) (genReg r)]
  genAssignInst r (MemRead loc) = [triple "movl" (genLoc loc) (genReg r)]
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
  genAssignInst cx@(CXR c) (CompRHS (Comp l@(RegL1S _)    op r@(RegL1S _))) =
    genCompInst cx r l (setInstruction op)
  genAssignInst cx@(CXR c) (CompRHS (Comp l@(NumberL1S _) op r@(RegL1S _))) =
    -- magic reverse happens here!
    genCompInst cx l r (foldOp "setg" "setge" "sete" op)
  genAssignInst cx@(CXR c) (CompRHS (Comp l@(RegL1S _)    op r@(NumberL1S _))) =
    genCompInst cx r l (setInstruction op)
  genAssignInst cx@(CXR _) (CompRHS (Comp l@(NumberL1S n1) op r@(NumberL1S n2))) =
    [triple "movl" ("$" ++ (if (runOp op n1 n2) then "1" else "0")) (genReg cx)]
  genAssignInst (CXR Eax) (Print s) = [
    "pushl " ++ genS s,
    "call print",
    "addl $4, %esp" ]
  genAssignInst (CXR Eax) (Allocate s n) = [
    "pushl " ++ genS n,
    "pushl " ++ genS s,
    "call allocate",
    "addl $8, %esp" ]
  genAssignInst (CXR Eax) (ArrayError s n) = [
    "pushl " ++ genS n,
    "pushl " ++ genS s,
    "call print_error",
    "addl $8, %esp" ]
  -- todo: should i bother changing the return type to Either[String, [X86Inst]]?
  genAssignInst l r = error $ "bad assignment statement: " ++ show (Assign l r)
  
  genCompInst cx@(CXR c) l r x = [
    triple "cmp" (genS l) (genS r),
    x ++ " " ++ (low8 c),
    triple "movzbl" (low8 c) (genReg cx) ]
    where low8 cx = "%" ++ [show cx !! 1] ++ "l"
  
  declare label = "L1_" ++ label ++ ":"
  triple op s1 s2 = op ++ " " ++ s1 ++ ", " ++ s2
  genReg :: Register -> String
  genReg (CXR cx) = "%" ++ show cx
  genReg (XR x)   = "%" ++ show x
  genS :: L1S -> String
  genS (NumberL1S i) = "$" ++ show i
  genS (LabelL1S  l) = "$L1_" ++ l
  genS (RegL1S    r) = genReg r
  --genS (RegL1S    r) = "%" ++ (show r)
  genLoc (MemLoc r i) = concat [show i, "(", genReg r, ")"]
  
  jump :: L1S -> String
  jump (LabelL1S name) = "jmp L1_" ++ name
  jump l               = "jmp *" ++ (genS l)
  
  setInstruction = foldOp "setl" "setle" "sete"
  
  jumpIfLess            l = "jl L1_"  ++ l
  jumpIfLessThanOrEqual l = "jle L1_" ++ l
  jumpIfGreater         l = "jg L1_"  ++ l
  jumpIfGreaterOrEqual  l = "jge L1_" ++ l
  jumpIfEqual           l = "je L1_"  ++ l
  
compileL1 = genX86Code . parseL1 . sread

main = do  
   fileNames <- getArgs
   -- just read the first file here.
   -- i suppose later on we could compile many files...
   let inputFile = fileNames !! 0
   let outputFile = changeExtension inputFile "S"
   results <- fmap compileL1 (readFile inputFile)
   writeFile outputFile results
