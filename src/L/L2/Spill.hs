module L.L2.Spill
  (
  ) where

import Control.Monad.State
import L.L1L2AST

todo = error "todo"
spillPrefix = "spilled_var_"

-- Spill a variable
--   spillVar is obviously the variable to spill
--   stackOffset is TODO (i don't remember what this is!)
--   memLoc is the location in memory to spill the variable
spill :: Variable -> Int -> L2MemLoc -> L2Instruction -> State Int [L2Instruction]
spill spillVar stackOffset memLoc = spillI where

  spillVarX = VarL2X spillVar
  readSpillVar = MemRead memLoc
  writeSpillVar v = MemWrite memLoc (XL2S v)

  newVar :: State Int L2X
  newVar = do
    n <- get
    _ <- put (n + 1)
    return $ VarL2X $ spillPrefix ++ (show n)

  withNewVar :: (L2X -> a) -> State Int a
  withNewVar f = fmap f newVar

  spillI (Assign x rhs)             = todo
  spillI (MathInst x op s)          = spillMathOp x op s
  spillI (MemWrite (MemLoc bp _) s) = todo 
  spillI g@(Goto _)                 = return [g]
  spillI (CJump (Comp s1 _ s2) _ _) = todo 
  spillI l@(LabelDeclaration _)     = return [l]
  spillI (Call s)                   = todo 
  spillI (TailCall s)               = todo 
  spillI r@Return                   = return [r]
  
  spillX (RegL2X _) = todo 
  spillX (VarL2X v) = todo 
  
  spillS (XL2S x)        = todo 
  spillS (NumberL2S n)   = todo 
  spillS (LabelL2S l)    = todo 
  
  spillRHS (CompRHS (Comp s1 _ s2)) = todo 
  spillRHS (Allocate s1 s2)         = todo 
  spillRHS (Print s)                = todo 
  spillRHS (ArrayError a n)         = todo 
  spillRHS (MemRead (MemLoc bp _))  = todo 

  spillMathOp :: L2X -> X86Op -> L2S -> State Int [L2Instruction]
  spillMathOp x op s 
    -- (x += x) for any op, where x is being spilled (x is now in memory)
    -- read x from memory and store it in the new var
    -- do the original computation using the new var instead of x
    -- write the value of the new var back to x's loc in memory 
    | (x == spillVarX && XL2S x == s) = withNewVar $ \v ->
      [Assign v readSpillVar, MathInst v op (XL2S v), writeSpillVar v]  
    -- (x += somethingElse) for any op, where x is being spilled (x is now in memory)
    -- read x from memory and store it in the new var
    -- do the original computation using the new var instead of x
    -- write the value of the new var back to x's loc in memory 
    | x == spillVarX = withNewVar $ \v ->
      [Assign v readSpillVar, MathInst v op s, writeSpillVar v]
    -- (y += x) for any op, where x is being spilled (x is now in memory)
    -- read x from memory and store it in the new var
    -- do the original computation using the new var instead of x
    -- this updates y's value, so we don't have to write x back to memory
    | s == XL2S spillVarX = withNewVar $ \v -> 
      [Assign v readSpillVar, MathInst x op (XL2S v)]
    -- spill var is not in the instruction, so just return it.
    | otherwise = return [MathInst x op s]

