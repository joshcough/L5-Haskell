{-# LANGUAGE TupleSections #-}
module L.L2.L2 where

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
import L.CompilationUnit
import L.IOHelpers
import L.L1L2AST
import L.L1L2Parser
import L.Read
import L.Utils
import L.L1.L1 (compileL1AndRunNative)
import L.L1.L164
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

compileL264 :: String -> Either String L164
compileL264 code = genL1Code <$> parseL264 (sread code) where
  --genL1Code :: L2 -> L1
  genL1Code (Program main fs) = 
    -- TODO: in scala, I stripped off the main label if it was present
    -- in the main function. Why did I do that? Do I need to here?
    Program (allocate True main) $ (allocate False <$> fs)

compileL264OrDie :: String -> L164
compileL264OrDie = (either error id) . compileL264

compileL264ToX86 :: String -> String
compileL264ToX86 code = either error id $ compileL264 code >>= genX8664Code

--compileL264File :: IO ()
--compileL264File = compile compileL264OrDie "S"
compileL264File_ :: FilePath -> IO L164
compileL264File_ = compile1 compileL264OrDie

compileL2FileAndRunNative :: FilePath -> FilePath -> IO String
compileL2FileAndRunNative l2File outputDir = do
  l1 <- compileL264File_ l2File
  _  <- writeFile l1File (show l1)
  compileL1AndRunNative l1 (Just l2File) outputDir where
  l1File = changeDir (changeExtension l2File "L1") outputDir

interpL2String :: String -> Either String String
interpL2String code = showOutput . interpL1 <$> compileL264 code

interpL2OrDie :: String -> String
interpL2OrDie = (either error id) . interpL2String

interpL2File =
  do s <- compile_ $ (either error id) . interpL2String
     putStrLn (snd s)

-- gives back a fully allocated function (if its possible to allocate it)
-- with all of the variables replaced with the assigned registers.
allocate :: (Num a, Ord a, Show a) => Bool -> L2Func a -> L1Func a
allocate isMain f = let
    ((allocatedFunction, allocs), espOffset) = allocateCompletely f 
    -- adjust the stack at the start of the function right here.
    label = allocatedFunction !! 0
    bodyWithoutLabel = tail allocatedFunction
    decEsp :: Num a => L2Instruction a
    decEsp = MathInst (RegL2X esp) decrement (NumberL2S (- (fromIntegral espOffset)))
    incEspMaybe :: Num a => [L2Instruction a]
    incEspMaybe = if isMain 
                  then [MathInst (RegL2X esp) increment (NumberL2S (- (fromIntegral espOffset)))] 
                  else []
    finalFunction = Func $ concat [[label], [decEsp], bodyWithoutLabel, incEspMaybe]
  in replaceVarsWithRegisters allocs finalFunction

allocateCompletely :: (Num a, Ord a, Show a) => L2Func a -> (([L2Instruction a], Map Variable Register), Int)
allocateCompletely originalF = let
  -- TODO: there are no order to these variables
  -- need to pick based on # connections, and liveness length
  varsToSpill = nub $ body originalF >>= (Set.toList . vars)
  finalState = runState (go 0 (\i -> varsToSpill !! i) (body originalF)) 0
  -- TODO: the second thing is the offset...
  -- but it probably has to be adjusted somehow
  offset = snd finalState
  in (traceA $ fst finalState, offset)

go :: (Num a, Ord a, Show a) => 
  Int -> 
  (Int -> Variable) -> 
  [L2Instruction a] -> 
  State Int ([L2Instruction a], Map Variable Register)
go offset nextVarF insts =
  let g = buildInterferenceGraph $ liveness insts
  in case attemptAllocation g of
    Just registerMap -> return (insts, registerMap)
    Nothing -> spillDef (nextVarF offset, offset) insts >>= go (offset + 1) nextVarF

{-
  OLD STUFF THAT USED foldM.
  --nrVarsToSpill = length varsToSpill
  --espOffset = (- nrVarsToSpill * 8) - (if even nrVarsToSpill then 0 else 8)
  --finalState = runState (foldM (flip spillDef) (body originalF) (zipWithIndex varsToSpill)) 0
  --finalBody = fst runState
  --finalOffset = snd runState
  --g = buildInterferenceGraph $ liveness finalBody
  --in case attemptAllocation g of
  --  Just registerMap -> traceA ((Func newBody, registerMap), espOffset)
  --  Nothing -> error "allocation impossible"
 -} 

-- TODO: this stuff should be someplace better than this
registers = [eax, ebx, ecx, edx, edi, esi] 
regSet = Set.fromList registers
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

-- sets up the function so that edi and esi can be spilled.
initialRewrite :: L2Func a -> L2Func a
initialRewrite f = let
  z1In  = Assign (VarL2X "__z1") $ SRHS $ (XL2S . RegL2X) edi
  z2In  = Assign (VarL2X "__z2") $ SRHS $ (XL2S . RegL2X) esi
  z1Out = Assign (RegL2X edi)    $ SRHS $ (XL2S . VarL2X) "__z1"
  z2Out = Assign (RegL2X esi)    $ SRHS $ (XL2S . VarL2X) "__z2"
  -- this business arranges to make sure that edi and esi
  -- get put back properly before a return or a tail-call.
  returnAdjustment :: L2Instruction a -> [L2Instruction a]
  returnAdjustment r@Return       = [z1Out, z2Out, r]
  returnAdjustment t@(TailCall _) = [z1Out, z2Out, t]
  returnAdjustment i              = [i]
  label                = body f !! 0
  insts                = drop 1 $ body f
  in Func $ concat [[label], [z1In,z2In], (insts >>= returnAdjustment)]
