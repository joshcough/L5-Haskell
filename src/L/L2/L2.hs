module L.L2.L2 where

import Data.List
import qualified Data.Map as M
import qualified Data.Set as S
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
compileL2 code = parseL2 (sread code) >>= genL1Code

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

allocateCompletely :: L2Func -> ((L2Func, M.Map Variable Register), Int)
allocateCompletely originalF = let
  -- TODO: looks like this just spills everything :(
  varsToSpill = nub $ body originalF >>= (S.toList . vars)
  spill_ b (sv, i) = spillDef sv (-i * 4) b
  newBody = foldl spill_ (body originalF) (zipWithIndex varsToSpill)
  g = buildInterferenceGraph $ liveness newBody
  in case attemptAllocation g of
    Just registerMap -> ((Func newBody, registerMap), (- (length varsToSpill) * 4))
    Nothing -> error "allocation impossible"

attemptAllocation :: Interference -> Maybe (M.Map Variable Register)
attemptAllocation = error "todo"

{--
  // the second thing returned here is the progress we were actually able to make.
  def attemptAllocation(iioss:List[InstructionInOutSet]):
    (Option[Map[Variable, Register]], Map[Variable, Option[Register]]) = {
    attemptAllocation(buildInterferenceSet(iioss))
--}


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
  returnAdjustment t@(TailCall s) = [z1Out, z2Out, t]
  returnAdjustment i              = [i]
  label                = body f !! 0
  insts                = drop 1 $ body f
  in Func $ concat [[label], [z1In,z2In], (insts >>= returnAdjustment)]

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
