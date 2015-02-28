{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module L.L2.Interference 
  (
    connections
   ,Interference(..)
   ,InterferenceGraph
   ,buildInterferenceGraph
   ,runInterference
   ,runInterferenceMain
   ,runInterferenceMain_
  ) where

import Data.List (sort)
import Data.Maybe
import qualified Data.Map as M
import qualified Data.Set as S
import L.IOHelpers (withFileArg,mapFileContents)
import L.L1L2AST
import L.Read (showAsList)
import L.Registers
import L.Utils (mkString)
import L.L2.Liveness
import L.L2.Vars
 
type InterferenceGraph = M.Map L2X (S.Set L2X)
newtype Interference = Interference InterferenceGraph

instance HasVars (S.Set L2X) where
  vars s = S.fromList [ v | VarL2X v <- S.toList s ]

instance HasVars Interference where
  vars (Interference g) = vars $ variables g

instance HasVars InterferenceGraph where
  vars g = vars $ variables g

empty :: InterferenceGraph
empty = M.empty

union :: InterferenceGraph -> InterferenceGraph -> InterferenceGraph
union = M.unionWith S.union

connections :: L2X -> InterferenceGraph -> S.Set L2X
connections x g = fromMaybe S.empty (M.lookup x g)

singleton :: L2X -> InterferenceGraph
singleton x = M.singleton x S.empty

graphMember :: L2X -> InterferenceGraph -> Bool
graphMember = M.member

graphMembers :: InterferenceGraph -> S.Set L2X
graphMembers = M.keysSet

-- adds a node to the graph, with nothing interfering with it
addNode :: L2X -> InterferenceGraph -> InterferenceGraph
addNode  x  g = if (graphMember x g) then g else (M.insert x S.empty g)
addNodes :: [L2X] -> InterferenceGraph -> InterferenceGraph
addNodes xs g = foldl (flip addNode) g xs

unions ::[InterferenceGraph] -> InterferenceGraph
unions gs = foldl union empty gs

addEdges :: [(L2X, L2X)] -> InterferenceGraph -> InterferenceGraph
addEdges edges g = foldl (flip addEdge) g edges where
  addEdge :: (L2X, L2X) -> InterferenceGraph -> InterferenceGraph
  addEdge (x1, x2) g
    -- dont bother adding ebp or esp
    | x1 == rbp = g
    | x1 == rsp = g
    | x2 == rbp = g
    | x2 == rsp = g
    | x1 == x2  = g -- dont add edge between a variable or register and itself...duh
    | otherwise = unions [g, singletonEdge x1 x2] where
    singletonEdge :: L2X -> L2X -> InterferenceGraph
    singletonEdge x1 x2
      | x1 == x2 = singleton x1
      | otherwise = M.fromList [(x1,S.singleton x2) ,(x2,S.singleton x1)]
  
mkGraph :: [(L2X, L2X)] -> InterferenceGraph
mkGraph edges = addEdges edges empty

edgeSetToGraph :: S.Set (L2X, L2X) -> InterferenceGraph
edgeSetToGraph edges = addEdges (S.toList edges) empty

variables :: InterferenceGraph -> S.Set L2X
variables = S.filter isVariable . graphMembers

registerInterference :: InterferenceGraph
registerInterference = mkGraph $ do [(x,y) | x <- allocatableRegisters, y <- allocatableRegisters]

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

      specialInterference :: InterferenceGraph
      specialInterference = mkGraph $ f (inst iios) where
        -- if you have this instruction (a <- b cmp c) then
        -- add edges between a and the registers rdi, rsi, rbp
        -- ensuring a ends up in another register, or spilled
        -- rdi, rsi and rbp aren't legal for comparison operations.
        f (Assign v@(VarL2X _) (CompRHS _)) = [(v, rdi), (v, rsi), (v, rbp)]
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
runInterferenceMain_ = mapFileContents runInterference

-- reads first command line argument, loads that file
-- calls runLiveness on it, shows it, and returns it.
runInterferenceMain :: IO ()
runInterferenceMain = withFileArg $ \f ->
  mapFileContents runInterference f >>= (putStrLn . show)
