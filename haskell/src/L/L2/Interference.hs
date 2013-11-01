module L.L2.Interference where

import L.L1L2AST

import Control.Monad
import Data.Maybe
import qualified Data.Map as Map
import qualified Data.Set as Set

type InterferenceGraph = Map.Map L2X (Set.Set L2X)
data InstructionInOutSet = InstructionInOutSet {
  index  :: Int, 
  inst   :: L2Instruction, 
  gen    :: Set.Set L2X, 
  kill   :: Set.Set L2X, 
  inSet  :: Set.Set L2X, 
  outSet :: Set.Set L2X 
}
type IIOS = InstructionInOutSet

emptyGraph :: InterferenceGraph
emptyGraph = Map.empty

connections :: AsL2X x => x -> InterferenceGraph -> Set.Set L2X
connections x g = fromMaybe Set.empty (Map.lookup (asL2X x) g)

insertOrAdd :: AsL2X x => x -> InterferenceGraph -> InterferenceGraph
insertOrAdd x g = Map.singleton (asL2X x) $ connections x g

graphMember :: AsL2X x => x -> InterferenceGraph -> Bool
graphMember = Map.member . asL2X

graphMembers :: InterferenceGraph -> Set.Set L2X
graphMembers = Map.keysSet

-- adds a node to the graph, with nothing interfering with it
addNode  g x  = if (graphMember x g) then g else (Map.insert x Set.empty g)
addNodes g xs = foldl addNode g xs

unionAll :: Ord k => [Map.Map k a] -> Map.Map k a
unionAll ms = foldl Map.union Map.empty ms

addEdge :: AsL2X x => (x, x) -> InterferenceGraph -> InterferenceGraph
addEdge (x1, x2) g
  -- dont bother adding ebp or esp
  | (asL2X x1) == (RegL2X ebp) = g
  | (asL2X x1) == (RegL2X esp) = g
  | (asL2X x2) == (RegL2X ebp) = g
  | (asL2X x2) == (RegL2X esp) = g
  | x1 == x2  = g -- dont add edge between a variable or register and itself...duh
  | otherwise = unionAll [g, (insertOrAdd x1 g), (insertOrAdd x2 g)]

addEdges :: AsL2X x => [(x, x)] -> InterferenceGraph -> InterferenceGraph
addEdges edges g = foldl (flip addEdge) g edges

isVariable :: L2X -> Bool
isVariable (VarL2X _) = True
isVariable _ = False

variables :: InterferenceGraph -> Set.Set L2X
variables = Set.filter isVariable . graphMembers

neigborsOf :: L2X -> InterferenceGraph -> Set.Set L2X
neigborsOf x g = fromMaybe (Set.empty) (Map.lookup x g)

registerInterference :: InterferenceGraph
registerInterference = 
  addEdges [
    (eax, ebx), (eax, ecx), (eax, edi), (eax, edx), (eax, esi),
    (ebx, ecx), (ebx, edi), (ebx, edx), (ebx, esi),
    (ecx, edi), (ecx, edx), (ecx, esi),
    (edi, edx), (edi, esi),
    (edx, esi)
  ] emptyGraph

zipFilter :: (a -> b -> Bool) -> [a] -> [b] -> [(a, b)]
zipFilter f as bs = filter (uncurry f) (zip as bs)

zipFilterSets :: (Ord a, Ord b) => (a -> b -> Bool) -> Set.Set a -> Set.Set b -> Set.Set (a, b)
zipFilterSets f xs ys = Set.fromList (zipFilter f (Set.elems xs) (Set.elems ys))

interference :: (Ord a, Ord b) =>
                (a -> b -> Bool) -> (c -> Set.Set a) -> (c -> Set.Set b) -> c  -> Set.Set (a, b)
interference f s1 s2 iios = zipFilterSets f (s1 iios) (s2 iios)

{-
  Build interference graph from the liveness information
    Two variables live at the same time interfere with each other
    Killed variables interfere with variables in the out set
    Except that the variables x and y do not interfere if the instruction was (x <- y)
    All real registers interfere with each other
-}
buildInterferenceSet :: [IIOS] -> InterferenceGraph
buildInterferenceSet iioss = 
  let 
    -- if there is a first instruction (i certainly imagine there should be)
    -- then we have to take the interference from its in set.
    firstInstructionInSetInterference :: Set.Set (L2X, L2X)
    firstInstructionInSetInterference = 
      maybe (Set.empty) (interference (/=) inSet outSet) (listToMaybe iioss)

    -- we always take the interference from the out sets.
    outAndSpecialInterference :: [(L2X, L2X)]
    outAndSpecialInterference =
      let outAndSpecialInterference1 :: IIOS -> [(L2X, L2X)]
          outAndSpecialInterference1 iios = 
            -- add in the kill
            let outsPlusKill = Set.union (outSet iios) (kill iios)
                initial = zipFilterSets (<) outsPlusKill outsPlusKill
                --assignmentRemovals (Assign (VarL2X v1) (SRHS (RegL2S r))) = 
                --  Just $ if (v < x) then (v, x) else (x, v)
                --assignmentRemovals (Assign (VarL2X v1) (SRHS (VarL2S r))) = 
                --  Just $ if (v < x) then (v, x) else (x, v)
            in error "todo"
      in iioss >>= outAndSpecialInterference1
  in
  error "todo"


{--

  def buildInterferenceSet(iioss: List[IIOS]): InterferenceGraph = {

    // we always take the interference from the out sets.
    val outAndSpecialInterference: List[(X,X)] = iioss.flatMap { iios: IIOS =>
      val out_interference: Set[(X,X)] = {
        // add in the kill
        val outsPlusKill = (iios.out ++ iios.kill)
        val initial = for(x <- outsPlusKill; y <- outsPlusKill; if(x<y)) yield (x,y)
        val assignmentRemovals = iios.inst match {
          case Assignment(v1:Variable, x:X) if(v1 != x) => Some(if(v1<x) (v1, x) else (x, v1))
          case Assignment(r: Register, v: Variable) => Some(if(r<v) (r, v) else (v, r))
          case _ => None
        }
        assignmentRemovals match {
          case Some(r) => initial -- Set(r)
          case _ => initial
        }
      }
      //  Constrained arithmetic operators
      //  Add interference edges to disallow the illegal registers
      //  when building the interference graph, before starting the coloring.
      val special_interference: Set[(X, X)] = iios.inst match {
        // if you have this instruction (a <- y < x) then
        // add edges between a and the registers edi and esi,
        // ensuring a ends up in eax, ecx, edx, ebx, or spilled
        // The (cx <- s cmp s) instruction in L1 is limited to only 4 possible destinations.
        case Assignment(v:Variable, _:Comp) => Set((v, edi), (v, esi))
        // The (x sop= sx) instruction in L1 is limited to only
        // shifting by the value of ecx (or by a constant in the other form)
        case LeftShift(_,  x:X) => Set((x, eax), (x, ebx), (x, edi), (x, edx), (x, esi))
        case RightShift(_, x:X) => Set((x, eax), (x, ebx), (x, edi), (x, edx), (x, esi))
        case _ => Set()
      }
      out_interference ++ special_interference
    }

    val interference = firstInstructionInSetInterference  ++ outAndSpecialInterference.toSet

    val vs = iioss.flatMap { iios => variables(iios.inst) }
    registerInterference.addNodes(vs:_*).addEdges(interference.toList:_*)
  }

  def variables(i:Instruction): Set[Variable] = {
    def variables(rhs:AssignmentRHS): Set[Variable] = rhs match {
      case v:Variable => Set(v)
      case Print(s) => variables(s)
      case Allocate(n, init) => variables(n)  union variables(init)
      case ArrayError(a, n)  => variables(a)  union variables(n)
      case Comp(s1, op, s2)  => variables(s1) union variables(s2)
      case MemRead(MemLoc(bp, _)) => variables(bp)
      case r:Register => Set()
      case n:Num => Set()
      case l:Label => Set()
    }
    i match {
      case Assignment(x, i) => variables(x) union variables(i)
      case Increment(x, s)  => variables(x) union variables(s)
      case Decrement(x, s)  => variables(x) union variables(s)
      case Multiply(x, s)   => variables(x) union variables(s)
      case LeftShift(x, s)  => variables(x) union variables(s)
      case RightShift(x, s) => variables(x) union variables(s)
      case BitwiseAnd(x, s) => variables(x) union variables(s)
      case CJump(comp, l1, l2) => variables(comp)
      case MemWrite(MemLoc(bp, _), s) => variables(bp) union variables(s)
      case Call(s) => variables(s)
      case TailCall(s) => variables(s)
      case Goto(_) => Set()
      case Return => Set()
      case LabelDeclaration(_) => Set()
    }
  }
}

  /**
    Example:
    ((eax ebx ecx edi edx esi x)
    (ebx eax ecx edi edx esi)
    (ecx eax ebx edi edx esi)
    (edi eax ebx ecx edx esi x)
    (edx eax ebx ecx edi esi)
    (esi eax ebx ecx edi edx x)
    (x eax edi esi))
   */
  def hwView = {
    def sortedMembers = members.toList.sorted
    def sortedNeighborNames(x:X): List[String] = map(x).toList.sorted.map(L2Printer.toCode)
    sortedMembers.map{ m =>
      (L2Printer.toCode(m) :: sortedNeighborNames(m)).mkString("(", " ", ")")
    }.mkString("(", "\n", ")")
  }

- -}
