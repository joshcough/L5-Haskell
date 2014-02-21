{-# LANGUAGE TupleSections #-}
module L.L2.L2
  (
    compileL2
   ,compileL2OrDie
   ,compileL2FileAndRunNative
   ,interpL2File
  )
where

import Control.Applicative
import Control.Lens
import Control.Monad.State
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Traversable
import Debug.Trace
import L.CompilationUnit
import L.IOHelpers
import L.L1L2AST
import L.L1L2Parser
import L.Read
import L.Utils
import L.L1.L1 (compileL1AndRunNative)
import L.L1.L1X86
import L.L1.L1Interp
import L.L2.Interference
import L.L2.Liveness
import L.L2.Spill
import L.L2.Vars

-- L2 introduces variables on top of L1.
-- the L2 compiler is really just a register allocator.
-- for each function in the program, it tries to see if it can allocate it as is.
-- if so, great, assign variables to registers.
-- if it is unable to, it spills a variable, and tries again.
-- after that, it continuously tries to allocate the function.
-- it does this until either a) it works, or b) it is out of variables to spill.
-- the last case results in error.

compileL2 :: String -> Either String L1
compileL2 code = genL1Code <$> parseL2 (sread code) where
  --genL1Code :: L2 -> L1
  genL1Code (Program main fs) =
    let mainWithRet = Func (body main ++ [Return])
    in Program (allocate mainWithRet) $ (allocate <$> fs)

compileL2OrDie :: String -> L1
compileL2OrDie = (either error id) . compileL2

compileL2ToX86 :: String -> String
compileL2ToX86 code = either error id $ compileL2 code >>= genX86Code

--compileL2File :: IO ()
--compileL2File = compile compileL2OrDie "S"
compileL2File_ :: FilePath -> IO L1
compileL2File_ = compile1 compileL2OrDie

compileL2FileAndRunNative :: FilePath -> FilePath -> IO String
compileL2FileAndRunNative l2File outputDir = do
  l1 <- compileL2File_ l2File
  _  <- writeFile l1File (show l1)
  compileL1AndRunNative l1 (Just l2File) outputDir where
  l1File = changeDir (changeExtension l2File "L1") outputDir

interpL2String :: String -> Either String String
interpL2String code = showOutput . interpL1 <$> compileL2 code

interpL2OrDie :: String -> String
interpL2OrDie = (either error id) . interpL2String

interpL2File =
  do s <- compile_ $ (either error id) . interpL2String
     putStrLn (snd s)

-- gives back a fully allocated function (if its possible to allocate it)
-- with all of the variables replaced with the assigned registers.
allocate :: L2Func -> L1Func
allocate f = let
    ((allocatedFunction, allocs), rspOffset) = allocateCompletely f

    label = case allocatedFunction !! 0 of
      l@(LabelDeclaration x) -> l
      _           -> LabelDeclaration "main"

    -- make sure the stack only ever moves in by 16, because mac requires that.
    offset = 8 + (if even rspOffset then rspOffset * 8 else rspOffset * 8 + 8)
    decEsp = MathInst (RegL2X rsp) decrement (NumberL2S (fromIntegral offset))
    incEsp = MathInst (RegL2X rsp) increment (NumberL2S (fromIntegral offset))

    rewriteReturns insts = insts >>= f where
      f r@Return       = [incEsp, r]
      f i              = [i]

    finalFunction = Func $ concat [
      [label],
      [decEsp],                                -- adjust the stack at the start of the function.
      rewriteReturns $ tail allocatedFunction] -- adjust the stack upon returning from the function.
  in replaceVarsWithRegisters allocs finalFunction

allocateCompletely :: L2Func -> (([L2Instruction], Map Variable Register), Int)
allocateCompletely originalF = let
  finalState = runState (go 0 (body originalF)) 0
  -- TODO: the second thing is the offset...
  -- but it probably has to be adjusted somehow
  in (fst finalState, snd finalState)

go :: Int -> [L2Instruction] -> State Int ([L2Instruction], Map Variable Register)
go offset insts =
  let (Interference g) = buildInterferenceGraph $ liveness insts
  in case attemptAllocation (Interference g) of
    Just registerMap -> return (insts, registerMap)
    Nothing ->
      -- find the next variable to spill by figuring out which one has the most connections
      -- TODO: tie should go to the one with the longest liverange.
      let vs = Set.toList $ vars g
          s (v1, n1) (v2, n2) = compare n1 n2
          f (v, _) = not $ isPrefixOf defaultSpillPrefix v
          conns = filter f $ reverse $ sortBy s $ fmap (\v -> (v, Set.size $ connections (VarL2X v) g)) vs
          v = fst $ head conns
      in spillDef (v, offset * 8) insts >>= go (offset + 1)

{-
  OLD STUFF THAT USED foldM.
  --nrVarsToSpill = length varsToSpill
  --rspOffset = (- nrVarsToSpill * 8) - (if even nrVarsToSpill then 0 else 8)
  --finalState = runState (foldM (flip spillDef) (body originalF) (zipWithIndex varsToSpill)) 0
  --finalBody = fst runState
  --finalOffset = snd runState
  --g = buildInterferenceGraph $ liveness finalBody
  --in case attemptAllocation g of
  --  Just registerMap -> traceA ((Func newBody, registerMap), rspOffset)
  --  Nothing -> error "allocation impossible"
 -} 

-- TODO: this stuff should be someplace better than this
regSet = Set.fromList allocatableRegisters
getRegisters s = Set.fromList [ r | RegL2X r <- Set.toList s ]

attemptAllocation :: Interference -> Maybe (Map Variable Register)
attemptAllocation i@(Interference g) = let 
    vs = vars g
    pairings :: Map Variable Register
    pairings = foldl (findMatch g) Map.empty (Set.toList vs) -- TODO: sort list here
    unpairedVars :: Set Variable
    unpairedVars = vs Set.\\ Map.keysSet pairings
  in if Set.null unpairedVars then Just pairings else Nothing

-- try to assign a register to a variable
--   if its not possible, just return the input map
--   if it is, return the input map + (v, r)
findMatch :: InterferenceGraph -> Map Variable Register -> Variable -> Map Variable Register
findMatch g pairs v = maybe pairs (\r -> Map.insert v r pairs) choice where
  -- everything v conflicts with (reisters and variables)
  conflicts    :: Set L2X
  conflicts    = connections (_Variable # v) g
  -- just the variables v conflicts with
  varConflicts :: Set Variable
  varConflicts = vars conflicts
  -- just the registers v conflicts with
  regConflicts :: Set Register
  regConflicts = getRegisters $ conflicts Set.\\ Set.map VarL2X varConflicts
  -- all the registers v does NOT conflict with
  nonConflictingRegisters :: Set Register
  nonConflictingRegisters =  regSet Set.\\ regConflicts
  -- the set of registers that are already 
  -- taken by variables that v conflicts with
  registersConflictingVarsLiveIn :: Set Register
  registersConflictingVarsLiveIn = Set.fromList $ do
    v <- Set.toList varConflicts
    maybe [] (\a -> [a]) $ Map.lookup v pairs
  -- the available registers come from taking 
  -- the registers it doesnt conflict with, and removing those
  -- that are already taken by variables v conflicts with
  -- this set might be empty, meaning there is no register for v.
  --   (also meaning the graph is not colorable)
  availableRegisters :: Set Register
  availableRegisters = nonConflictingRegisters Set.\\ registersConflictingVarsLiveIn
  -- choose a register for v, if one is available.
  choice :: Maybe Register
  choice = listToMaybe $ Set.toList availableRegisters -- TODO: sort list here.
