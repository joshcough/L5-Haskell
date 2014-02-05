{-# LANGUAGE TupleSections #-}
module L.L2.L2 where

import Control.Applicative
import Control.Lens
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Traversable
import L.L1L2AST
import L.L1L2Parser
import L.Read
import L.Utils
import L.L2.Interference
import L.L2.Liveness
import L.L2.Spill
import L.L2.Vars

-- L2 introduces variables on top of L1.
-- the L2 compiler is really just a register allocator.
-- for each function in the program, it tries to see if it can allocate it as is.
-- if so, great, assign variables to registers.
-- if not, it rewrites it so that edi and esi can be spilled.
--    TODO: i dont think that comment is true... i just always spill edi and esi...
-- after that, it continuously tries to allocate the function.
-- if it is unable to, it spills a variable, and tries again.
-- it does this until either a) it works, or b) it is out of variables to spill.
-- the last case results in error.

compileL2 :: String -> Either String L1
compileL2 code = genL1Code <$> parseL264 (sread code) where
  genL1Code :: L2 -> L1
  genL1Code (Program main fs) = 
    -- TODO: in scala, I stripped off the main label if it was present
    -- in the main function. Why did I do that? Do I need to here?
    Program (allocate True main) $ (allocate False <$> fs)

-- gives back a fully allocated function (if its possible to allocate it)
-- with all of the variables replaced with the assigned registers.
allocate :: Bool -> L2Func -> L1Func
allocate isMain f = let
    ((allocatedFunction, allocs), espOffset) = allocateCompletely (initialRewrite f)
    -- adjust the stack at the start of the function right here.
    label = body allocatedFunction !! 0
    bodyWithoutLabel = tail $ body allocatedFunction
    decEsp :: L2Instruction
    decEsp = MathInst (RegL2X esp) decrement (NumberL2S (- (fromIntegral espOffset)))
    incEspMaybe :: [L2Instruction]
    incEspMaybe = if isMain 
                  then [MathInst (RegL2X esp) increment (NumberL2S (- (fromIntegral espOffset)))] 
                  else []
    finalFunction = Func $ concat [[label], [decEsp], bodyWithoutLabel, incEspMaybe]
  in replaceVarsWithRegisters allocs finalFunction

allocateCompletely :: L2Func -> ((L2Func, Map Variable Register), Int)
allocateCompletely originalF = let
  -- TODO: looks like this just spills everything :(
  varsToSpill = nub $ body originalF >>= (Set.toList . vars)
  spill_ b (sv, i) = spillDef sv (-i * 8) b
  newBody = foldl spill_ (body originalF) (zipWithIndex varsToSpill)
  g = buildInterferenceGraph $ liveness newBody
  in case attemptAllocation g of
    Just registerMap -> 
      ((Func newBody, registerMap), (- (length varsToSpill) * 8))
    Nothing -> error "allocation impossible"

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
initialRewrite :: L2Func -> L2Func
initialRewrite f = let
  z1In  = Assign (VarL2X "__z1") $ SRHS $ (XL2S . RegL2X) edi
  z2In  = Assign (VarL2X "__z2") $ SRHS $ (XL2S . RegL2X) esi
  z1Out = Assign (RegL2X edi)    $ SRHS $ (XL2S . VarL2X) "__z1"
  z2Out = Assign (RegL2X esi)    $ SRHS $ (XL2S . VarL2X) "__z2"
  -- this business arranges to make sure that edi and esi
  -- get put back properly before a return or a tail-call.
  returnAdjustment :: L2Instruction -> [L2Instruction]
  returnAdjustment r@Return       = [z1Out, z2Out, r]
  returnAdjustment t@(TailCall _) = [z1Out, z2Out, t]
  returnAdjustment i              = [i]
  label                = body f !! 0
  insts                = drop 1 $ body f
  in Func $ concat [[label], [z1In,z2In], (insts >>= returnAdjustment)]
