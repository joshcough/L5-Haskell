module L.L3.Linearize (linearize) where

import Control.Applicative
import Control.Monad.State hiding (sequence, (>>))
import Data.Int
import Data.Traversable
import L.L1L2AST  as L2
import L.L3.L3AST as L3
import Prelude hiding (EQ, LT, sequence, (>>))

-- TODO: some of these are also in other code.
argRegisters :: [Register]
argRegisters = [rdi, rsi, rdx, rcx, r8, r9]
l2True :: L2S
l2True = NumberL2S 3
regRHS :: Register -> AssignRHS x L2S
regRHS = SRHS . XL2S . RegL2X
toLHS :: Register -> L2X
toLHS = RegL2X
(<~) :: x -> AssignRHS x s -> Instruction x s
(<~) = Assign
(+=), (-=), (*=), (>>), (<<), (&) :: x -> s -> Instruction x s
(+=) x s = MathInst x Increment  s
(-=) x s = MathInst x Decrement  s
(*=) x s = MathInst x Multiply   s
(>>) x s = MathInst x RightShift s
(<<) x s = MathInst x LeftShift  s
(&)  x s = MathInst x BitwiseAnd s
num :: Int64 -> L2S
num = NumberL2S
newTemp  :: State Int L2X
newTemp  = VarL2X <$> newId
newLabel :: State Int String
newLabel = (':':) <$> newId
newId :: State Int String
newId = do { n <- get; put (n + 1); return $ "_tempL3_" ++ show n }
memRead :: Variable -> AssignRHS L2X L2S
memRead v = MemRead (MemLoc (VarL2X v) 0)
compileError :: String -> a
compileError msg = error $ "L3 compile error: " ++ msg

linearize :: L3 -> L2
linearize l3 = fst $ runState (linearizeS l3) 0

linearizeS :: L3 -> State Int L2
linearizeS (L3 e funcs) = (L2.Program callMain) <$> l2Funcs where
  callMain :: L2.L2Func
  callMain = L2.Func [LabelDeclaration ":main", L2.Call $ LabelL2S ":__L3main__", Return]
  l2MainInsts = (LabelDeclaration ":__L3main__" :) <$> compileE e
  l2Funcs :: State Int [L2.L2Func]
  l2Funcs  = liftM2 (:) (L2.Func <$> l2MainInsts) (traverse compileFunction funcs)

-- (l (x ...) e)
-- there is an implicit assertion here that L3 functions
-- can have at most 6 arguments,
-- because there are 6 x86-64 argument registers
compileFunction :: L3.Func -> State Int L2.L2Func
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
  var       <- newTemp
  thenLabel <- newLabel
  elseLabel <- newLabel
  v'        <- compileD (VD v) var
  t'        <- compileE t
  f'        <- compileE f
  -- todo: make sure test is correct. this is going to require a bunch of testing.
  let test    = CJump (Comp (compileV v) L2.EQ l2True) thenLabel elseLabel
      thenDec = LabelDeclaration thenLabel
      elseDec = LabelDeclaration elseLabel
  return $ concat [v', [test], [thenDec], t', [elseDec], f']
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
compileD (PrimApp Mult [l, r]) dest = do { tmp <- newTemp; return
  [tmp  <~ compileVRHS l, tmp >> num 1, dest <~ compileVRHS r,
   dest >> num 1, dest *= XL2S tmp, dest << num 1, dest += num 1]}
compileD (PrimApp LessThan [l, r]) dest = return $ compileComp l r dest LT
compileD (PrimApp LTorEQ   [l, r]) dest = return $ compileComp l r dest LTEQ
compileD (PrimApp EqualTo  [l, r]) dest = return $ compileComp l r dest EQ
compileD (PrimApp IsNumber [v])    dest = return $
  [dest <~ compileVRHS v, dest &  num 1, dest << num 1, dest += num 1]
compileD (PrimApp IsArray [v]) dest = return $
  [dest <~ compileVRHS v, dest &  num 1, dest *= num (-2), dest += num 3]
compileD (PrimApp NewArray [size, init]) dest = return $
  [toLHS rax <~ Allocate (compileV size) (compileV init), dest <~ regRHS rax]
compileD (PrimApp ALen [VarV v]) dest = return [ dest <~ memRead v, dest << num 1, dest += num 1 ]
{-
  (x <- (mem s n4))
  NOTE: s/basePointer must be a Var, because NumVs and LabelVs can't be arrays.
  TODO: if indexV is a NumV, we can definitely handle things more efficiently.
-}
compileD (PrimApp ARef (NumV   n : _)) _ = compileError $ "aref called with a number: " ++ show n
compileD (PrimApp ARef (LabelV l : _)) _ = compileError $ "aref called with a label: "  ++ l
compileD (PrimApp ARef [VarV basePointer, indexV]) dest = do
  -- Instead of creating a fresh variable to store the mem pointer,
  -- just store it in the destination instead. The let is only for clarity.
  let index = dest
  compileIndexInsts <- return $ compileIndex indexV index
  boundsCheckInsts  <- checkArrayBounds basePointer index
  addressCalcInsts  <- return $ calculateAddress basePointer index
  let aref  = dest  <~ MemRead (MemLoc index 0) -- no offset needed, because we have the exact address.
  return $ concat [ compileIndexInsts, boundsCheckInsts, addressCalcInsts, [aref] ]
-- ((mem x n4) <- s)
-- (let ([x (aset v1 v2 v3)]) ...)
compileD (PrimApp ASet (NumV   n : _)) _ = compileError $ "aref called with a number: " ++ show n
compileD (PrimApp ASet (LabelV l : _)) _ = compileError $ "aref called with a label: "  ++ l
compileD (PrimApp ASet [VarV basePointer, indexV, newVal]) dest = do
  let index = dest
  compileIndexInsts <- return $ compileIndex indexV index
  boundsCheckInsts  <- checkArrayBounds basePointer index
  addressCalcInsts  <- return $ calculateAddress basePointer index
  let aset = [ MemWrite (MemLoc index 0) (compileV newVal), dest  <~ SRHS (num 1) ]
  return $ concat [ compileIndexInsts, boundsCheckInsts, addressCalcInsts, aset ]
compileD (VD v)            dest = return $ [dest <~ compileVRHS v]
compileD (NewTuple vs)     dest = return $ newTuple (fmap compileV vs) dest
compileD (ClosureProc v)   dest = compileD (PrimApp ARef [v, NumV 0]) dest
compileD (ClosureVars v)   dest = compileD (PrimApp ARef [v, NumV 1]) dest
compileD (MakeClosure l v) dest = do
  temp <- newTemp
  let assignment = temp <~ SRHS (LabelL2S l)
  return $ assignment : newTuple [XL2S temp, compileV v] dest
compileD bad dest = compileError $ "bad L3-D: " ++ show bad ++ ", dest: " ++ show dest

newTuple :: [L2S] -> L2X -> [L2Instruction]
newTuple as dest =
  concat [[arr], writes, [dest <~ regRHS rax]] where
    arr    = rax <~ Allocate (compileV . NumV $ fromIntegral $ length as) (num 1)
    writes = fmap f (zip as [1..])
    f (a, i) = MemWrite (MemLoc rax (i * 8)) a

compileComp :: V -> V -> L2X -> CompOp -> [L2Instruction]
compileComp l r dest op = 
  [dest <~ compileVRHS l,
   dest <~ CompRHS (Comp (XL2S dest) op (compileV r)),
   dest << num 1,
   dest += num 1]

compileV :: V -> L2S
compileV (VarV v)   = XL2S $ VarL2X v
compileV (NumV i)   = NumberL2S (i*2+1)
compileV (LabelV l) = LabelL2S l

compileVRHS :: V -> AssignRHS L2X L2S
compileVRHS = SRHS . compileV
-- if there is a destination (L2X) then make a regular Call
-- storing the result of the call (which is in rax) into the destination
-- otherwise, make a TailCall
compileFunCall :: V -> [V] -> Maybe L2X -> [L2Instruction] 
compileFunCall v vs dest = regAssignments ++ call where
  -- on a function call, we need to put the arguments into the registers
  regAssignments = zipWith Assign (RegL2X <$> argRegisters) (SRHS . compileV <$> vs)
  v'   = compileV v
  call = maybe ([TailCall v']) (\d -> [Call v', Assign d $ regRHS rax]) dest

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

checkArrayBounds :: Variable -> L2X -> State Int [L2Instruction]
checkArrayBounds basePointer index = do
  size               <- newTemp
  boundsFailLabel    <- newLabel
  boundsPassLabel    <- newLabel
  checkNegativeLabel <- newLabel
  return [
    -- Read the size out of the array - notice that it's in the 0 position.
    size  <~ memRead basePointer,
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
