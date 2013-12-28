{-# LANGUAGE TupleSections #-}

module L.L2.L2 where

import Control.Applicative
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

compileL2 :: String -> Either String L1
compileL2 code = genL1Code <$> parseL2 (sread code)

-- TODO: in scala, I stripped off the main label if it was present
-- in the main function. Why did I do that? Do I need to here?
genL1Code :: L2 -> L1
genL1Code (Program main fs) = 
  Program (allocate True main) $ (allocate False <$> fs)

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
allocate :: Bool -> L2Func -> L1Func
allocate isMain f = let
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

allocateCompletely :: L2Func -> ((L2Func, Map Variable Register), Int)
allocateCompletely originalF = let
  -- TODO: looks like this just spills everything :(
  varsToSpill = nub $ body originalF >>= (Set.toList . vars)
  spill_ b (sv, i) = spillDef sv (-i * 4) b
  newBody = foldl spill_ (body originalF) (zipWithIndex varsToSpill)
  g = buildInterferenceGraph $ liveness newBody
  in case attemptAllocation g of
    Just registerMap -> ((Func newBody, registerMap), (- (length varsToSpill) * 4))
    Nothing -> error "allocation impossible"

registers = [eax, ebx, ecx, edx, edi, esi] 
regSet = Set.fromList registers
getRegisters s = Set.fromList [ r | RegL2X r <- Set.toList s ]

attemptAllocation :: Interference -> Maybe (Map Variable Register)
attemptAllocation i@(Interference g) =
  let vs = vars i
      pairings :: Map Variable Register
      pairings = foldl findPair Map.empty (Set.toList vs) -- TODO: sort list here
      findPair :: Map Variable Register -> Variable -> Map Variable Register
      findPair pairs v =
        let conflicts    :: Set L2X
            conflicts    = connections v g
            varConflicts :: Set Variable
            varConflicts = vars conflicts
            regConflicts :: Set Register
            regConflicts = getRegisters $ conflicts Set.\\ Set.map VarL2X varConflicts
            nonConflictingRegisters :: Set Register
            nonConflictingRegisters =  regSet Set.\\ regConflicts
            registersConflictingVarsLiveIn :: Set Register
            registersConflictingVarsLiveIn = Set.fromList $ do
              v <- Set.toList varConflicts
              maybe [] (\a -> [a]) $ Map.lookup v pairs
            availableRegisters :: Set Register
            availableRegisters = nonConflictingRegisters Set.\\ registersConflictingVarsLiveIn 
            choice :: Maybe Register
            choice = listToMaybe $ Set.toList availableRegisters -- TODO: sort list here.
        in maybe pairs (\r -> Map.insert v r pairs) choice  
      unpairedVars :: Set Variable
      unpairedVars = vs Set.\\ Map.keysSet pairings
  in if Set.null unpairedVars then Just pairings else Nothing

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
