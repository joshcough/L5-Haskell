{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, NoMonomorphismRestriction #-}

module L.L2.Liveness
  (
    InstructionInOutSet(..)
   ,LiveRange(..)
   ,IIOS
   ,liveness
   ,livenessMain_
   ,livenessMain
   ,liveRanges
   ,runLiveness
   ,showLiveness
  ) where

import Control.Lens hiding (index)
import Control.Monad
import Data.Int
import Data.List
import Data.Maybe
import qualified Data.Map as M
import qualified Data.Set as S
import L.L1L2AST
import L.L1L2Parser
import L.IOHelpers (mapFileContents, withFileArg)
import L.Read
import L.Utils

data InstructionInOutSet = InstructionInOutSet {
  index  :: Int,       inst    :: L2Instruction,
  genSet :: S.Set L2X, killSet :: S.Set L2X,
  inSet  :: S.Set L2X, outSet  :: S.Set L2X
} deriving (Eq)

type IIOS = InstructionInOutSet

showLiveness :: [IIOS] -> String
showLiveness is = 
  let go :: (IIOS -> S.Set L2X) -> String
      go f = mkString " " $ fmap (showAsList . fmap show) (fmap (S.toList . f) is) 
  in "((in " ++ go inSet ++ ") (out " ++ go outSet ++ "))" 

calleeSave      = S.fromList [rbx, rbp, r12, r13, r14]
callerSave      = S.fromList [rax, rcx, rdx, rsi, rdi, r8, r9, r10, r11]
arguments       = S.fromList [rdi, rsi, rdx, rcx, r8, r9]
returnRegisters = S.fromList [rax]

-- generate the gen set for an instruction
-- (what things are alive during that instruction)
gen :: L2Instruction -> S.Set L2X
gen i = genI i where

  genI :: L2Instruction -> S.Set L2X
  genI (Assign x rhs)             = genRHS rhs
  genI (MathInst x _ s)           = S.unions [genX x,  genS s]
  genI (MemWrite (MemLoc bp _) s) = S.unions [genX bp, genS s]
  genI (Goto s)                   = S.empty
  genI (CJump (Comp s1 _ s2) _ _) = S.unions [genS s1, genS s2]
  genI (LabelDeclaration _)       = S.empty
  genI (Call s)                   = S.unions [genS s,  arguments]
  genI (TailCall s)               = S.unions [genS s,  arguments, calleeSave]
  genI Return                     = S.unions [returnRegisters, calleeSave]

  genX :: L2X -> S.Set L2X
  genX r = S.fromList [r]

  genS :: L2S -> S.Set L2X
  genS (XL2S x)        = genX x
  genS (NumberL2S n)   = S.empty
  genS (LabelL2S l)    = S.empty
  
  genRHS :: AssignRHS L2X L2S -> S.Set L2X
  genRHS (CompRHS (Comp s1 _ s2)) = S.unions [genS s1, genS s2]
  genRHS (Allocate n init)        = S.unions [genS n,  genS init]
  genRHS (Print s)                = genS s
  genRHS (ArrayError a n)         = S.unions [genS a,  genS n]
  genRHS (MemRead (MemLoc bp _))  = genX bp
  genRHS (SRHS s)                 = genS s

-- generate the kill set for an instruction
-- (everthing that is overwritten during that instruction)
kill :: L2Instruction -> S.Set L2X
kill (Assign x (Print s))         = S.insert x callerSave
kill (Assign x (Allocate a init)) = S.insert x callerSave
kill (Assign x (ArrayError a n))  = S.insert x callerSave
kill (Assign x _)                 = S.fromList [x]
kill (MathInst x _ _)             = S.fromList [x]
kill (Call s)                     = S.unions [callerSave, returnRegisters]
kill _                            = S.empty

-- calculate the liveness for a list of instructions
liveness :: [L2Instruction] -> [IIOS]
liveness = head . inout

-- calculate the liveness for a string containing a list of instructions
runLiveness :: String -> [IIOS]
runLiveness = liveness . extract . parseL2InstList . sread

-- calculate the liveness for a string containing a list of instructions
-- that string is read from the given filepath
livenessMain_ :: FilePath -> IO [IIOS]
livenessMain_ = mapFileContents runLiveness

-- reads first command line argument, loads that file
-- calls runLiveness on it, and prints the result.
livenessMain :: IO ()
livenessMain = withFileArg $ \f -> 
  mapFileContents (showLiveness . runLiveness) f >>= putStrLn

--withFileArg :: (FilePath -> IO ()) -> IO ()
--fileArgMain :: Show a => (String -> a) -> IO ()

-- builds up a giant list of all the intermediate inout results
-- robby starts out with a function, and empty in and out sets for each instruction
-- that is the first result in the list return here
-- then there is a result for each slide all the way down to the last slide
-- when things are finally complete (we've reached the fixed point)
inout :: [L2Instruction] -> [[IIOS]]
inout is =
  let 
      nrInstructions :: Int
      nrInstructions = length is
      instructionsWithIndex = zipWithIndex is
      -- TODO: i think this map is busted because there could be two equal instructions
      -- it's only used to find label declarations too... this should be easier.
      indeces = M.fromList instructionsWithIndex
      findLabelDecIndex :: Label -> Int
      findLabelDecIndex l = 
        maybe (error $ "no such label: " ++ l) id (M.lookup (LabelDeclaration l) indeces) 
      succIndeces :: [S.Set Int]
      succIndeces = fmap succIndeces_ instructionsWithIndex where
        succIndeces_ :: (L2Instruction, Int) -> S.Set Int
        succIndeces_ (i, n) = case i of
          Return                    -> S.empty
          TailCall _                -> S.empty
          Assign _ (ArrayError _ _) -> S.empty
          Goto label                -> S.singleton $ findLabelDecIndex label
          CJump _ l1 l2             -> S.fromList [findLabelDecIndex l1, findLabelDecIndex l2]
          -- we have to test that there is something after this instruction
          -- in case the last instruction is something other than return or cjump
          -- i think that in normal functions this doesn't happen but the hw allows it.
          _ -> if (nrInstructions > (n + 1)) then  S.singleton (n+1) else S.empty
      step :: [IIOS] -> [IIOS]
      step current = fmap step_ current where
        step_ :: IIOS -> IIOS
        step_ i =
              -- in(n) = gen(n-th-inst) ∪ (out (n) - kill(n-th-inst))
          let newIn :: S.Set L2X
              newIn = S.union (genSet i) ((outSet i) S.\\ (killSet i))
              -- out(n) = ∪{in(m) | m ∈ succ(n)}
              newOut :: S.Set L2X
              newOut = S.fromList $ (fmap (current !!) $ S.toList (succIndeces !! (index i))) >>= S.toList . inSet
          in InstructionInOutSet (index i) (inst i) (genSet i) (killSet i) newIn newOut
      -- does the next round of moving things up the in/out chains.
      -- recurs until the result is the same as what we've got so far.
      inout_ :: [[IIOS]] -> [[IIOS]]
      inout_ acc =
        let current = acc !! 0
        --let current = traceA (showLiveness $ acc !! 0) $ acc !! 0
            nextStep = step current
        in if (nextStep == current) then acc else inout_ (nextStep : acc)
      -- start out with empty in and out sets for all instructions
      --emptyStartSet :: (Eq a, Ord a) => [IIOS]
      emptyStartSet = fmap f instructionsWithIndex where
        f (inst, index) = InstructionInOutSet index inst (gen inst) (kill inst) S.empty S.empty
  -- then fill them in until we reach the fixed point.
  in inout_ [emptyStartSet]

-- TODO: not using usages just yet, but it should be used in ties.
data LiveRange = LiveRange { x :: L2X, range :: Int, usages :: Int }
lr x r = LiveRange x r 0
instance Eq LiveRange where
  (==) (LiveRange _ r1 _) (LiveRange _ r2 _) = r1 == r2
instance Ord LiveRange where 
  compare (LiveRange _ r1 _) (LiveRange _ r2 _) = compare r1 r2

liveRanges :: [IIOS] -> [[LiveRange]]
liveRanges iioss =
  let inSets :: [S.Set L2X]
      inSets = fmap inSet iioss
      varsAndRegisters :: [L2X]
      varsAndRegisters = sort . S.toList $ foldl f S.empty inSets where
        f acc s = error "todo"
-- val variablesAndRegisters = inSets.foldLeft(Set[X]()){
--   -      case (acc, s) => acc union s
--   -      -    }.filterNot(x => x == ebp || x == esp)
--   -
      liveRanges1 :: L2X -> [S.Set L2X] -> [LiveRange]
      liveRanges1 _ [] = []
      liveRanges1 x sets@(s:_)
        | S.member x s = case span (S.member x) sets
          of (h, t) -> (lr x $ length h) : liveRanges1 x t
        | otherwise    = liveRanges1 x $ dropWhile (not . S.member x) sets 
  in fmap (flip liveRanges1 inSets) varsAndRegisters
