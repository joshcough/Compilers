{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module L.L2.Interference where

import Control.Monad
import Data.Maybe
import qualified Data.Map as M
import qualified Data.Set as S
import L.L1L2AST
import L.L2.Liveness
 
type InterferenceGraph = M.Map L2X (S.Set L2X)
type IIOS = InstructionInOutSet

empty :: InterferenceGraph
empty = M.empty

union :: InterferenceGraph -> InterferenceGraph -> InterferenceGraph
union = M.unionWith S.union

connections :: AsL2X x => x -> InterferenceGraph -> S.Set L2X
connections x g = fromMaybe S.empty (M.lookup (asL2X x) g)

singleton :: AsL2X x => x -> InterferenceGraph
singleton x = M.singleton (asL2X x) S.empty

graphMember :: AsL2X x => x -> InterferenceGraph -> Bool
graphMember = M.member . asL2X

graphMembers :: InterferenceGraph -> S.Set L2X
graphMembers = M.keysSet

-- adds a node to the graph, with nothing interfering with it
addNode  x  g = if (graphMember x g) then g else (M.insert x S.empty g)
addNodes xs g = foldl (flip addNode) g xs

unions ::[InterferenceGraph] -> InterferenceGraph
unions gs = foldl union empty gs

addEdge :: (AsL2X x, AsL2X y) => (x, y) -> InterferenceGraph -> InterferenceGraph
addEdge (x1, x2) g
  -- dont bother adding ebp or esp
  | (asL2X x1) == (RegL2X ebp) = g
  | (asL2X x1) == (RegL2X esp) = g
  | (asL2X x2) == (RegL2X ebp) = g
  | (asL2X x2) == (RegL2X esp) = g
  | (asL2X x1) == (asL2X x2)   = g -- dont add edge between a variable or register and itself...duh
  | otherwise = unions [g, singletonEdge x1 x2] where
  singletonEdge :: (AsL2X x, AsL2X y) => x -> y -> InterferenceGraph
  singletonEdge x1 x2
    | (asL2X x1) == (asL2X x2) = singleton x1
    | otherwise = union
       (M.singleton (asL2X x1) (S.singleton (asL2X x2)))
       (M.singleton (asL2X x2) (S.singleton (asL2X x1)))

addEdges :: (AsL2X x, AsL2X y) => [(x, y)] -> InterferenceGraph -> InterferenceGraph
addEdges edges g = foldl (flip addEdge) g edges

mkGraph :: (AsL2X x, AsL2X y) => [(x, y)] -> InterferenceGraph
mkGraph edges = addEdges edges empty

edgeSetToGraph :: AsL2X x => S.Set (x, x) -> InterferenceGraph
edgeSetToGraph edges = addEdges (S.toList edges) empty

isVariable :: L2X -> Bool
isVariable (VarL2X _) = True
isVariable _ = False

variables :: InterferenceGraph -> S.Set L2X
variables = S.filter isVariable . graphMembers

registerInterference :: InterferenceGraph
registerInterference = mkGraph [
  (eax, ebx), (eax, ecx), (eax, edi), (eax, edx), (eax, esi),
  (ebx, ecx), (ebx, edi), (ebx, edx), (ebx, esi),
  (ecx, edi), (ecx, edx), (ecx, esi),
  (edi, edx), (edi, esi),
  (edx, esi) ]

zipFilterSets :: (Ord a, Ord b) => (a -> b -> Bool) -> S.Set a -> S.Set b -> S.Set (a, b)
zipFilterSets f xs ys = S.fromList (filter (uncurry f) $ zip (S.elems xs) (S.elems ys))

interference :: (Ord a, Ord b) =>
                (a -> b -> Bool) -> (c -> S.Set a) -> (c -> S.Set b) -> c -> S.Set (a, b)
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
    -- take the interference from the first instruction's in set.
    firstInstructionInSetInterference :: InterferenceGraph
    firstInstructionInSetInterference = 
      maybe empty (edgeSetToGraph . interference (/=) inSet outSet) (listToMaybe iioss)

    -- take the interference from all the out sets.
    outAndSpecialInterference :: InterferenceGraph
    outAndSpecialInterference = unions (fmap outAndSpecialInterference1 iioss)
   
    variables = iioss >>= ((fmap VarL2X) . S.toList . vars . inst)

  in unions [
    addNodes variables registerInterference, 
    firstInstructionInSetInterference, 
    outAndSpecialInterference
  ]

outAndSpecialInterference1 :: IIOS -> InterferenceGraph
outAndSpecialInterference1 iios =
  let outInterference :: InterferenceGraph
      outInterference = 
        -- add in the kill
        let outsPlusKill = S.union (outSet iios) (kill iios)
            initial = zipFilterSets (<) outsPlusKill outsPlusKill
            jop x1 x2 = Just $ orderedPair x1 x2
            assignmentRemovals (Assign v@(VarL2X _) (SRHS (XL2S x)))            = jop v x
            assignmentRemovals (Assign r@(RegL2X _) (SRHS (XL2S v@(VarL2X _)))) = jop r v
            assignmentRemovals _ = Nothing
        in edgeSetToGraph $ 
             maybe initial (S.difference initial . S.singleton) (assignmentRemovals (inst iios))

      -- Constrained arithmetic operators
      -- Add interference edges to disallow the illegal registers
      -- then building the interference graph, before starting the coloring.
      specialInterference :: InterferenceGraph
      specialInterference = mkGraph $ f (inst iios) where
        -- if you have this instruction (a <- y < x) then
        -- add edges between a and the registers edi and esi,
        -- ensuring a ends up in eax, ecx, edx, ebx, or spilled
        -- The (cx <- s cmp s) instruction in L1 is limited to only 4 possible destinations.
        f (Assign v@(VarL2X _) (CompRHS _)) = [(v, edi), (v, esi)]
        f (MathInst _ LeftShift  (XL2S x))  = [(x, eax), (x, ebx), (x, edi), (x, edx), (x, esi)]
        f (MathInst _ RightShift (XL2S x))  = [(x, eax), (x, ebx), (x, edi), (x, edx), (x, esi)]
        f _ = []
  in union outInterference specialInterference

class HasVars a where
  vars :: a -> S.Set Variable

instance HasVars L2X where
  vars (RegL2X _) = S.empty
  vars (VarL2X v) = S.singleton v

instance HasVars L2S where
  vars (XL2S x)        = vars x
  vars (NumberL2S n)   = S.empty
  vars (LabelL2S l)    = S.empty

instance HasVars (AssignRHS L2X L2S) where
  vars (CompRHS (Comp s1 _ s2)) = S.unions [vars s1, vars s2] 
  vars (Allocate s1 s2)         = S.unions [vars s1, vars s2] 
  vars (Print s)                = vars s
  vars (ArrayError a n)         = S.unions [vars a,  vars n]
  vars (MemRead (MemLoc bp _))  = vars bp
  vars (SRHS s)                 = vars s

instance HasVars L2Instruction where
  vars (Assign x rhs)             = S.unions [vars x,  vars rhs]
  vars (MathInst x _ s)           = S.unions [vars x,  vars s]
  vars (MemWrite (MemLoc bp _) s) = S.unions [vars bp, vars s]
  vars (Goto _)                   = S.empty
  vars (CJump (Comp s1 _ s2) _ _) = S.unions [vars s1, vars s2]
  vars (LabelDeclaration _)       = S.empty
  vars (Call s)                   = vars s
  vars (TailCall s)               = vars s
  vars Return                     = S.empty

{-
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
-}
