import System.Environment
import System.IO
import Data.List
import Read
import L1L2AST
import L1L2Parser
import IOHelpers
import Data.Traversable
import Data.Map
import Control.Monad.State
import Control.Monad.Error

-- L2 Parser (uses shared L1/L2 Parser)
parseL2 :: SExpr -> Either String L2
parseL2 = parse (parseI (parseX VarL2X RegL2X) parseL2S) where
  parseX v r  s = Right $ maybe (v $ drop 1 s) r (parseRegister s)
  parseL2S    s = case (sread s) of
    AtomNum n -> Right $ NumberL2S n
    AtomSym s -> maybe (parseX VarL2S RegL2S s) Right $ parseLabelOrRegister LabelL2S RegL2S s

-- todo: comment me
-- todo: maybe replace errors with Eithers - Map Variable Register -> L2Func -> Either String L1Func
-- the problem with that though, is that it really would be a coding error.
-- if the map isnt set up properly, something in other code is bad.
-- i could weave through an Either everywhere, but that really clutters the code.
-- is there a better way to reflect this in the type system?
replaceVarsWithRegisters :: Map Variable Register -> L2Func -> L1Func
replaceVarsWithRegisters replacements func = Func $ Data.List.map replaceInInst (body func) where
  replaceInInst :: L2Instruction -> L1Instruction
  replaceInInst (Assign x rhs)        = Assign (getRegister x) (replaceInRHS rhs)
  replaceInInst (MathInst x op s)     = MathInst (getRegister x) op (replaceInS s)
  replaceInInst (MemWrite   loc s)    = MemWrite (replaceInMemLoc loc) (replaceInS s)
  replaceInInst (Goto s)              = Goto s
  replaceInInst (CJump comp l1 l2)    = CJump (replaceInComp comp) l1 l2
  replaceInInst (Call s)              = Call $ replaceInS s
  replaceInInst (TailCall s)          = TailCall $ replaceInS s
  replaceInInst (LabelDeclaration ld) = LabelDeclaration ld
  replaceInInst Return                = Return

  replaceInS :: L2S -> L1S
  replaceInS (VarL2S v) = maybe (error "bad register") RegL1S $ Data.Map.lookup v replacements
  replaceInS (NumberL2S n)   = NumberL1S n
  replaceInS (LabelL2S n)    = LabelL1S n
  replaceInS (RegL2S r)      = RegL1S r

  replaceInRHS :: AssignRHS L2X L2S -> AssignRHS L1X L1S
  replaceInRHS (Allocate s1 s2)       = Allocate (replaceInS s1) (replaceInS s2)
  replaceInRHS (Print s)              = Print (replaceInS s)
  replaceInRHS (ArrayError s1 s2)     = ArrayError (replaceInS s1) (replaceInS s2)
  replaceInRHS (MemRead loc)          = MemRead (replaceInMemLoc loc)
  replaceInRHS (SRHS s)               = SRHS (replaceInS s)
  replaceInRHS (CompRHS comp)         = CompRHS (replaceInComp comp)

  replaceInMemLoc (MemLoc x off) = MemLoc (getRegister x) off
  replaceInComp (Comp s1 op s2)  = Comp   (replaceInS s1) op (replaceInS s2)

  getRegister :: L2X -> Register
  getRegister (VarL2X v) = maybe (error "bad register") id $ Data.Map.lookup v replacements
  getRegister (RegL2X r)      = r

genL1Code :: L2 -> Either String L1
genL1Code = error "todo"


-- the allocator brings together everything in L2
-- for each function in the program, it tries to see if it can allocate it as is.
-- if so, its fine, and leaves it alone.
-- if not, it rewrites it so that edi and esi can be spilled.
-- after that, it continuously tries to allocate the function.
-- if it is unable to, it spills a variable, and tries again.
-- it does this until either a) it works, or b) it is out of variables to spill.
-- the last case results in error.

-- gives back a fully allocated function (if its possible to allocate it)
-- with all of the variables replaced with the assigned registers.
allocate :: L2Func -> Bool -> L1Func
allocate f isMain = let
  ((allocatedFunction, allocs), espOffset) = allocateCompletely (initialRewrite f)
  -- adjust the stack at the start of the function right here.
  label = body allocatedFunction !! 0
  bodyWithoutLabel = tail $ body allocatedFunction
  decEsp :: L2Instruction
  decEsp = MathInst (RegL2X esp) decrement (NumberL2S (- espOffset))
  incEspMaybe :: [L2Instruction]
  incEspMaybe = if isMain then [ MathInst (RegL2X esp) increment (NumberL2S (- espOffset)) ] else []
  finalFunction = Func $ concat [[label], [decEsp], bodyWithoutLabel, incEspMaybe]
  in replaceVarsWithRegisters allocs finalFunction

allocateCompletely = error "todo"

-- sets up the function so that edi and esi can be spilled.
initialRewrite :: L2Func -> L2Func
initialRewrite f = let
  z1In  = Assign (VarL2X "__z1") $ SRHS $ RegL2S edi
  z2In  = Assign (VarL2X "__z2") $ SRHS $ RegL2S esi
  z1Out = Assign (RegL2X edi)    $ SRHS $ VarL2S "__z1"
  z2Out = Assign (RegL2X esi)    $ SRHS $ VarL2S "__z2"
  -- this business arranges to make sure that edi and esi
  -- get put back properly before a return or a tail-call.
  returnAdjustment :: L2Instruction -> [L2Instruction]
  returnAdjustment r@Return       = [z1Out, z2Out, r]
  returnAdjustment t@(TailCall s) = [z1Out, z2Out, t]
  returnAdjustment i              = [i]
  label                = body f !! 0
  insts                = drop 1 $ body f
  in Func $ concat [[label], [z1In,z2In], (insts >>= returnAdjustment)]

compileL2 :: String -> Either String L1
compileL2 code = parseL2 (sread code) >>= genL1Code

-- just read the first file here. i suppose later on i could compile many files...
main = fmap (!! 0) getArgs >>= compileToFile where
  compileToFile inputFile =
    let outputFile = changeExtension inputFile "L1" in
    fmap ((either error id) . compileL2) (readFile inputFile) >>= (writeFile outputFile . show)

{--
  def compile(ast:L2): L2 = {
    val l1 = timed("allocate", allocate(ast))
    val mainWithoutLabel = l1.main.body.headOption match {
      case Some(l) if l == mainLabel || l == mainLabelDec => Func(l1.main.body.tail)
      case _ => l1.main
    }
    L2(mainWithoutLabel,l1.funs)
  }

    // allocates all of the functions in the given L2 program
    def allocate(ast: L2): L2 = {
      val newMain = allocate(ast.main, true)
      val l1Functions = ast.funs.map(f => timed("allocating function: " + f.name, allocate(f, false)))
      L2(newMain, l1Functions)
    }


-- dont remove this yet. it might be a better way to handle the var->register mapping above
-- and it is definitely true if we need to so something over the same structure again.
mapS :: (Variable -> L1S) -> L2S -> L1S
mapS f (VarL2S v) = f v
mapS _ (NumberL2S n)   = NumberL1S n
mapS _ (LabelL2S n)    = LabelL1S n
mapS _ (RegL2S r)      = RegL1S r

mapFunc :: (L2S -> L1S) -> (L2X -> L1X) -> L2Func -> L1Func
mapFunc sf xf func = Func $ Data.List.map mapInst (body func) where
  mapInst :: L2Instruction -> L1Instruction
  mapInst (Assign x rhs)        = Assign (xf x) (mapRHS rhs)
  mapInst (MathInst x op s)     = MathInst (xf x) op (mapS s)
  mapInst (MemWrite   loc s)    = MemWrite (mapMemLoc loc) (mapS s)
  mapInst (Goto s)              = Goto s
  mapInst (CJump comp l1 l2)    = CJump (mapComp comp) l1 l2
  mapInst (Call s)              = Call $ mapS s
  mapInst (TailCall s)          = TailCall $ mapS s
  mapInst (LabelDeclaration ld) = LabelDeclaration ld
  mapInst Return                = Return

  mapRHS :: AssignRHS L2X L2S -> AssignRHS L1X L1S
  mapRHS (Allocate s1 s2)       = Allocate (mapS s1) (mapS s2)
  mapRHS (Print s)              = Print (mapS s)
  mapRHS (ArrayError s1 s2)     = ArrayError (mapS s1) (mapS s2)
  mapRHS (MemRead loc)          = MemRead (mapMemLoc loc)
  mapRHS (SRHS s)               = SRHS (mapS s)
  mapRHS (CompRHS comp)         = CompRHS (mapComp comp)

  mapMemLoc (MemLoc x off) = MemLoc (xf x) off
  mapComp (Comp s1 op s2)  = Comp   (mapS s1) op (mapS s2)
 --}