{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module L.L2.Interference 
  (
    connections
   ,Interference(..)
   ,InterferenceGraph(..)
   ,buildInterferenceGraph
   ,runInterference
   ,runInterferenceMain
   ,runInterferenceMain_
  ) where

import Control.Monad
import Data.List (sort)
import Data.Maybe
import qualified Data.Map as M
import qualified Data.Set as S
import Debug.Trace
import L.CompilationUnit
import L.IOHelpers (withFileArg)
import L.L1L2AST
import L.Read (showAsList)
import L.Utils (mkString, traceA)
import L.L2.Liveness
import L.L2.Vars
 
type InterferenceGraph = M.Map L2X (S.Set L2X)
newtype Interference = Interference InterferenceGraph

instance HasVars (S.Set L2X) where
  vars s = S.fromList [ v | VarL2X v <- S.toList s ]

instance HasVars InterferenceGraph where
  vars g = vars $ variables g

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

addEdges :: (AsL2X x, AsL2X y) => [(x, y)] -> InterferenceGraph -> InterferenceGraph
addEdges edges g = foldl (flip addEdge) g edges where
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
  
mkGraph :: [(L2X, L2X)] -> InterferenceGraph
mkGraph edges = addEdges edges empty

edgeSetToGraph :: S.Set (L2X, L2X) -> InterferenceGraph
edgeSetToGraph edges = addEdges (S.toList edges) empty

variables :: InterferenceGraph -> S.Set L2X
variables = S.filter isVariable . graphMembers

registerInterference :: InterferenceGraph
registerInterference = mkGraph [
  (eax, ebx), (eax, ecx), (eax, edi), (eax, edx), (eax, esi),
  (ebx, ecx), (ebx, edi), (ebx, edx), (ebx, esi),
  (ecx, edi), (ecx, edx), (ecx, esi),
  (edi, edx), (edi, esi),
  (edx, esi) ]

{-
  Build interference graph from the liveness information
    Two variables alive at the same time interfere with each other
    (that means they cannot live in the same register)
    Killed variables interfere with variables in the out set
    Except that the variables x and y do not interfere if the instruction was (x <- y)
    All real registers interfere with each other
-}
buildInterferenceGraph :: [IIOS] -> Interference
buildInterferenceGraph iioss = 
  let 
    -- start with a graph containing all of the variables and registers
    -- but with no edges (no interference)
    variables = iioss >>= ((fmap VarL2X) . S.toList . vars . inst)
    variableAndRegisterGraph = addNodes variables registerInterference

    -- take the interference from the first instruction's in set.
    firstInstructionInSetInterference :: InterferenceGraph
    firstInstructionInSetInterference = 
      maybe empty f (listToMaybe iioss) where
        f i = let l = S.toList (inSet i)
              in edgeSetToGraph $ S.fromList $ [(x,y) | x <- l, y <- l, x < y]

    -- take the interference for each instruction.
    -- (which is from the out sets, and some special cases)
    outAndSpecialInterference :: InterferenceGraph
    outAndSpecialInterference = unions (fmap interference1 iioss)

  in Interference $ unions [
    variableAndRegisterGraph, 
    firstInstructionInSetInterference, 
    outAndSpecialInterference
  ]

interference1 :: IIOS -> InterferenceGraph
interference1 iios =
  let outInterference :: InterferenceGraph
      outInterference = 
        -- start with the outSet unioned with the killSet
        -- because anything that is alive (out) in the instruction
        -- interferes with anything killed in the instruction
        -- (because if it didn't interfere, it could be possible that
        --  it gets into a register that gets killed)
        let outsPlusKill :: S.Set L2X
            outsPlusKill = S.union (outSet iios) (killSet iios)
            initial :: S.Set (L2X, L2X)
            initial = S.fromList $ let l = S.toList outsPlusKill in [(x,y) | x <- l, y <- l, x < y]
            jop :: L2X -> L2X -> Maybe (L2X, L2X)
            jop x1 x2 = Just $ orderedPair x1 x2
            -- there is one exception to that rule, however - assignment statements
            -- x <- y
            -- x gets killed here, but doesn't interfere with y
            -- (unless x gets used again later, but then x and y will
            --  interfere because of different rules)
            assignmentRemovals :: L2Instruction -> Maybe (L2X, L2X)
            assignmentRemovals (Assign v@(VarL2X _) (SRHS (XL2S x)))            = jop v x
            assignmentRemovals (Assign r@(RegL2X _) (SRHS (XL2S v@(VarL2X _)))) = jop r v
            assignmentRemovals _ = Nothing
        in edgeSetToGraph $ maybe 
             initial 
             (S.difference initial . S.singleton) 
             (assignmentRemovals $ inst iios)

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

{-
  Example:
  ((eax ebx ecx edi edx esi x)
  (ebx eax ecx edi edx esi)
  (ecx eax ebx edi edx esi)
  (edi eax ebx ecx edx esi x)
  (edx eax ebx ecx edi esi)
  (esi eax ebx ecx edi edx x)
  (x eax edi esi))
-}
instance Show Interference where
  show (Interference g) = "(" ++ (mkString "\n") memberLists ++ ")" where
    sortedMembers = sort $ S.toList $ graphMembers g
    showMember x  = showAsList $ fmap show $ x : S.toList (connections x g)
    memberLists   = fmap showMember sortedMembers 

-- calculate the intereference graph for a string containing a list of instructions
runInterference :: String -> Interference
runInterference = buildInterferenceGraph . runLiveness

-- calculate the intereference graph for a string containing a list of instructions
-- that string is read from the given filepath
-- return a the result wrapped in a CompilationUnit for testing purposes
-- it allows the result file to be read.
runInterferenceMain_ :: FilePath -> IO Interference
runInterferenceMain_ = compile1 runInterference

-- reads first command line argument, loads that file
-- calls runLiveness on it, shows it, and returns it.
runInterferenceMain :: IO ()
runInterferenceMain = withFileArg $ \f ->
  compile1 runInterference f >>= (putStrLn . show)
