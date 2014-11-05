module L.L3.Linearize (linearize) where

import Control.Applicative
import Control.Monad.State hiding (sequence, (>>))
import Data.Int
import Data.Traversable
import L.L1L2AST  as L2
import L.L3.L3AST as L3
import Prelude hiding (EQ, LT, sequence, (>>))

linearize :: L3 -> L2
linearize l3 = fst $ runState (linearizeM l3) 0

linearizeM :: L3 -> State Int L2
linearizeM (L3 e funcs) = (L2.Program callMain) <$> l2Funcs where
  callMain = L2.Func [LabelDeclaration ":main", L2.Call $ LabelL2S ":_L3main_", Return]
  l2MainInsts = (LabelDeclaration ":_L3main_" :) <$> compileE e
  l2Funcs  = liftM2 (:) (L2.Func <$> l2MainInsts) (traverse compileFunction funcs)

-- (l (x ...) e)
-- there is an implicit assertion here that L3 functions
-- can have at most 6 arguments,
-- because there are 6 x86-64 argument registers
compileFunction :: L3.L3Func -> State Int L2.L2Func
compileFunction (L3.Func name args body) = L2.Func <$> insts where
  labl = LabelDeclaration name
  -- inside a function body, we need to get the argument values out of the registers
  argAssignments = zipWith (<~) (VarL2X <$> args) (regRHS <$> argRegisters)
  insts :: State Int [L2Instruction]
  insts = do { e <- compileE body; return $ concat [[labl], argAssignments, e, [Return]] }

-- e ::= (let ([x d]) e) | (if v e e) | d
compileE :: E -> State Int [L2Instruction]
compileE (Let v d e) = liftM2 (++) (compileD d (VarL2X v)) (compileE e)
compileE (IfStatement v t f) = do
  (temp,thenLabel,elseLabel) <- liftM3 (,,) newTemp newLabel newLabel
  (v',t',f') <- liftM3 (,,) (compileD (VD v) temp) (compileE t) (compileE f)
  let test = CJump (Comp (compileV v) L2.EQ l2False) elseLabel thenLabel
  return $ concat [v', [test], [LabelDeclaration thenLabel], t', [LabelDeclaration elseLabel], f']
-- if it is an function call, make a tail call
compileE (DE (FunCall v vs)) = return $ compileFunCall v vs Nothing
-- otherwise, compile the d, store the result in rax, and return.
compileE (DE d) = (\d' -> d' ++ [Return]) <$> compileD d (RegL2X rax)

compileD :: D -> L2X -> State Int [L2Instruction]
compileD (PrimApp L3.Print [v]) dest =
  return [toLHS rax <~ (L2.Print $ compileV v), dest <~ regRHS rax]
-- TODO...tail-call if last d in the tree??
compileD (FunCall v vs) dest = return $ compileFunCall v vs (Just dest)
-- biop ::= + | - | * | < | <= | =
compileD (PrimApp Add [l, r]) dest = return $
  [dest <~ compileVRHS l, dest += compileV r, dest -= num 1]
compileD (PrimApp Sub [l, r]) dest = return $
  [dest <~ compileVRHS l, dest -= compileV r, dest += num 1]
compileD (PrimApp Mult [l, r]) dest = do
  tmp <- newTemp
  return $ [tmp  <~ compileVRHS l,
            tmp >> num 1,
            dest <~ compileVRHS r,
            dest >> num 1,
            dest *= XL2S tmp] ++ encode dest
compileD (PrimApp LessThan [l, r]) dest = return $ compileCompOp l r dest LT
compileD (PrimApp LTorEQ   [l, r]) dest = return $ compileCompOp l r dest LTEQ
compileD (PrimApp EqualTo  [l, r]) dest = return $ compileCompOp l r dest EQ
compileD (PrimApp IsNumber [v])    dest = return $
  [dest <~ compileVRHS v, dest &  num 1] ++ encode dest
compileD (PrimApp IsArray [v]) dest = return $
  [dest <~ compileVRHS v, dest &  num 1, dest *= num (-2), dest += num 3]
compileD (PrimApp NewArray [size, init]) dest = return $
  [toLHS rax <~ Allocate (compileV size) (compileV init), dest <~ regRHS rax]
 -- NOTE: for alen, aref, aset, basePointer must be a Var, because NumVs and LabelVs can't be arrays.
compileD (PrimApp ALen [VarV basePointer]) dest =
  return $ (dest <~ memRead basePointer) : encode dest
compileD (PrimApp ALen vs) _ = arrayCallError "alen" vs
-- (x <- (mem s n4))
-- TODO: if indexV is a NumV, we can definitely handle things more efficiently.
compileD (PrimApp ARef [VarV basePointer, indexV]) dest =
  -- no offset needed, because we have the exact address.
  withArrayIndex basePointer indexV dest $ \index -> [dest  <~ MemRead (MemLoc index 0)]
compileD (PrimApp ARef vs) _ = arrayCallError "aref" vs
-- ((mem x n4) <- s)
-- (let ([x (aset v1 v2 v3)]) ...)
compileD (PrimApp ASet [VarV basePointer, indexV, newVal]) dest =
  withArrayIndex basePointer indexV dest $ \index ->
    [ MemWrite (MemLoc index 0) (compileV newVal), dest  <~ SRHS (num 1) ]
compileD (PrimApp ASet vs)      _    = arrayCallError "aset" vs
compileD (VD v)                 dest = return $ [dest <~ compileVRHS v]
compileD (NewTuple vs)          dest = return $ newTuple (fmap compileV vs) dest
compileD (ClosureProc (VarV v)) dest = return $ [dest <~ MemRead (MemLoc (VarL2X v) 8)]
compileD (ClosureProc v)        _    = arrayCallError "closure-vars" [v]
compileD (ClosureVars (VarV v)) dest = return $ [dest <~ MemRead (MemLoc (VarL2X v) 16)]
compileD (ClosureVars v)        _    = arrayCallError "closure-proc" [v]
compileD (MakeClosure l v)      dest = do
  temp <- newTemp
  return $ (temp <~ SRHS (LabelL2S l)) : newTuple [XL2S temp, compileV v] dest
compileD bad dest = compileError $ concat ["bad L3-D: ", show bad, ", dest: ", show dest]

newTuple :: [L2S] -> L2X -> [L2Instruction]
newTuple as dest = concat [[arr], writes, [dest <~ regRHS rax]] where
  arr    = rax <~ Allocate (compileV . NumV $ fromIntegral $ length as) (num 1)
  writes = fmap f (zip as [1..])
  f (a, i) = MemWrite (MemLoc rax (i * 8)) a

compileCompOp :: V -> V -> L2X -> CompOp -> [L2Instruction]
compileCompOp l r dest op = 
  [dest <~ compileVRHS l, dest <~ CompRHS (Comp (XL2S dest) op (compileV r))] ++ encode dest

compileV :: V -> L2S
compileV = foldV (XL2S . VarL2X) (\i -> NumberL2S $ i*2+1) LabelL2S

compileVRHS :: V -> AssignRHS L2X L2S
compileVRHS = SRHS . compileV
{-
  if there is a destination (L2X)
   * then make a regular Call storing the result of the call (which is in rax) into the destination
   * otherwise, make a TailCall
-}
compileFunCall :: V -> [V] -> Maybe L2X -> [L2Instruction] 
compileFunCall v vs dest = regAssignments ++ call where
  -- on a function call, we need to put the arguments into the registers
  regAssignments = zipWith Assign (RegL2X <$> argRegisters) (SRHS . compileV <$> vs)
  call = maybe ([TailCall $ compileV v]) (\d -> [Call $ compileV v, Assign d $ regRHS rax]) dest

-- Used to do array bounds checking in aref and aset.
withArrayIndex :: Variable -> V -> L2X -> (L2X -> [L2Instruction]) -> State Int [L2Instruction]
withArrayIndex basePointer indexV dest finalInstsFromIndex = do
  compileIndexInsts <- return $ compileIndex indexV dest
  boundsCheckInsts  <- checkArrayBounds basePointer dest
  addressCalcInsts  <- return $ calculateAddress basePointer dest
  let finalInsts    =  finalInstsFromIndex dest
  return $ concat [ compileIndexInsts, boundsCheckInsts, addressCalcInsts, finalInsts ] where
  {-
    indexV is any arbitrary v, we need to compile it
    * if its a variable, it just stays a variable
        and presumably that variable had already been encoded at some point
    * if its a number, we encode it so that it's consistent with a variable

    But our checks are going to be against regular (non-encoded) numbers,
    because the size stored in the array is regular number.
    So, we unencode the number by shifting it right by 1 here.
  -}
  compileIndex :: V -> L2X -> [L2Instruction]
  compileIndex indexV index = [index <~ compileVRHS indexV, index >> num 1]
  {-
    * The index is currently a regular number, because we shifted it as explained above.
      When indexing into an array, we always have to add 1, because the size is at 0.
      (index 0 -> 1, index 1 -> 2, ... index n -> n + 1)
    * We multiply the index by 8 because we are storing 8 byte integers
    * Finally, we then add the base pointer to it to get the exact memory location.
  -}
  calculateAddress :: Variable -> L2X -> [L2Instruction]
  calculateAddress basePointer index =
    [ index += num 1, index *= num 8, index += (XL2S $ VarL2X basePointer) ]
  {-
    Checks to make sure the index is in the bounds of the array.
  -}
  checkArrayBounds :: Variable -> L2X -> State Int [L2Instruction]
  checkArrayBounds basePointer index = do
    (size,boundsFailLabel,boundsPassLabel,checkNegativeLabel) <- liftM4 (,,,) newTemp newLabel newLabel newLabel
    return [
      -- Read the size out of the array - notice that it's in the 0 position.
      size <~ memRead basePointer,
      {-
        Check to see if the index is in bounds
        if so, jump to boundsFailLabel,
        if not jump to checkNegativeLabel, which is where we check if the index is negative.
      -}
      CJump (Comp (XL2S size) LTEQ (XL2S index)) boundsFailLabel checkNegativeLabel,
      -- This is where we handle an index out of bounds error.
      LabelDeclaration boundsFailLabel,
      index << num 1,
      index += num 1,
      rax   <~ ArrayError (XL2S $ VarL2X basePointer) (XL2S index),
      -- This is where we check if the index is negative
      LabelDeclaration checkNegativeLabel,
      CJump (Comp (XL2S index) LT (num 0)) boundsFailLabel boundsPassLabel,
      LabelDeclaration boundsPassLabel ]

-- TODO: some of these are also in other code.
argRegisters :: [Register]
argRegisters = [rdi, rsi, rdx, rcx, r8, r9]
l2False :: L2S
l2False = NumberL2S 1
regRHS :: Register -> AssignRHS x L2S
regRHS = SRHS . XL2S . RegL2X
toLHS :: Register -> L2X
toLHS = RegL2X
(<~) :: x -> AssignRHS x s -> Instruction x s
(<~) = Assign
(+=), (-=), (*=), (>>), (<<), (&) :: x -> s -> Instruction x s
(+=) x = MathInst x Increment
(-=) x = MathInst x Decrement
(*=) x = MathInst x Multiply
(>>) x = MathInst x RightShift
(<<) x = MathInst x LeftShift
(&)  x = MathInst x BitwiseAnd
num :: Int64 -> L2S
num = NumberL2S
newTemp  :: State Int L2X
newTemp  = VarL2X <$> newId
newLabel :: State Int String
newLabel = (':':) <$> newId
newId :: State Int String
newId = do { n <- get; put (n + 1); return $ "_l3_" ++ show n }
memRead :: Variable -> AssignRHS L2X L2S
memRead v = MemRead (MemLoc (VarL2X v) 0)
compileError :: String -> a
compileError msg = error $ "L3 compile error: " ++ msg
arrayCallError :: String -> [V] -> a
arrayCallError typ vs = compileError $ typ ++ " called with a with bad arguments: " ++ show vs
encode :: L2X -> [L2Instruction]
encode dest = [dest << num 1, dest += num 1]