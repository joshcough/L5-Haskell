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
  spillVarS = XL2S spillVarX
  readSpillVarInto x = Assign x (MemRead memLoc)
  writeSpillVar v = MemWrite memLoc (XL2S v)

  newVar :: State Int L2X
  newVar = do
    n <- get
    _ <- put (n + 1)
    return $ VarL2X $ spillPrefix ++ (show n)

  withNewVar   :: (L2X -> a) -> State Int a
  withNewVar   f = fmap f newVar
  withNewVarR  :: (L2X -> [L2Instruction]) -> State Int [L2Instruction]
  withNewVarR  f = fmap (\v -> readSpillVarInto v : f v) newVar
  withNewVarRW :: (L2X -> [L2Instruction]) -> State Int [L2Instruction]
  withNewVarRW f = fmap (\v -> readSpillVarInto v : (f v ++ [writeSpillVar v])) newVar

  spillI (Assign x rhs)                 = todo
  spillI (CJump c@(Comp s1 _ s2) l1 l2) = spillCJump c l1 l2 
  spillI (MathInst x op s)              = spillMathInst x op s
  spillI (MemWrite loc s)               = spillMemWrite loc s
  spillI (Call s)
    | s == spillVarS = withNewVarR $ \v -> [Call (XL2S v)]
    | otherwise = return [Call s]
  spillI (TailCall s) 
    | s == spillVarS = withNewVarR $ \v -> [TailCall (XL2S v)]
    | otherwise = return [Call s]
  spillI g@(Goto _)                 = return [g]
  spillI l@(LabelDeclaration _)     = return [l]
  spillI r@Return                   = return [r]
  
  spillRHS (CompRHS (Comp s1 _ s2)) = todo 
  spillRHS (Allocate s1 s2)         = todo 
  spillRHS (Print s)                = todo 
  spillRHS (ArrayError a n)         = todo 
  spillRHS (MemRead (MemLoc bp _))  = todo 
  spillRHS (SRHS s)                 = todo

  spillAssignment :: L2X -> AssignRHS L2X L2S -> State Int [L2Instruction]
  -- assignments to variable from variable
  spillAssignment v1@(VarL2X _) rhs@(SRHS (XL2S v2@(VarL2X _)))
    -- if we have x <- x, just remove it.
    | v1 == spillVarX && v1 == v2 = return []
    -- x <- y where x is spillVar
    | v1 == spillVarX = return [MemWrite memLoc (XL2S v2)]
    -- y <- x where x is spillVar
    | v2 == spillVarX = return [readSpillVarInto v1]
    -- y <- z
    | otherwise = return [Assign v1 rhs]
  -- assignment to variable from register or number
  spillAssignment v@(VarL2X _) rhs@(SRHS s@(XL2S _))
    | v == spillVarX = return [MemWrite memLoc s]
    | otherwise      = return [Assign v rhs]
  -- assignment to register from variable
  spillAssignment r@(RegL2X _) rhs@(SRHS s@(XL2S v@(VarL2X _)))
    | v == spillVarX = return [MemWrite memLoc s]
    | otherwise      = return [readSpillVarInto r]
  -- assignment to register from register or number
  spillAssignment r@(RegL2X _) rhs@(SRHS _) = return [Assign r rhs]
  -- assignments with MemReads
  spillAssignment v1 read@(MemRead (MemLoc v2 off))
    -- funny case, x <- (mem x n) where x is spill var
    -- (s_0 <- (mem ebp stackOffset))
    -- (s_1 <- (mem s_0 off)
    -- ((mem ebp stackOffset) <- s_1)
    | v1 == v2 && v1 == spillVarX = withNewVarRW $ \v -> 
      [Assign v (MemRead (MemLoc v off))]
    -- x <- (mem y n)
    -- (s_0 <- (mem y n))
    -- ((mem ebp stackOffset) <- s_0)
    | v1 == spillVarX = withNewVar $ \v -> [Assign v read, writeSpillVar v]
    -- y <- (mem x n)
    -- (s_0 <- (mem ebp stackOffset))
    -- (y <- (mem s_0 n)
    | v2 == spillVarX = withNewVarR $ \v -> [Assign v1 (MemRead (MemLoc v off))]
    | otherwise = return [Assign v1 read]

  spillCJump c@(Comp s1 op s2) l1 l2
    -- (cjump x < x :l1 :l2)
    | s1 == spillVarS && s2 == spillVarS = f $ \v -> Comp (XL2S v) op (XL2S v)
    -- (cjump x < y  :l1 :l2)
    | s1 == spillVarS = f $ \v -> Comp (XL2S v) op s2
    -- (cjump y < x  :l1 :l2)
    | s2 == spillVarS = f $ \v -> Comp s1 op (XL2S v)
    | otherwise = return [CJump c l1 l2] where
    f cf = withNewVarR $ \v -> [CJump (cf v) l1 l2]

  -- if x or s is being spilled, 
  -- create a new var and read the spilled vars value from memory into it
  -- then do the original computation using the new var instead of x or s
  spillMathInst :: L2X -> X86Op -> L2S -> State Int [L2Instruction]
  spillMathInst x op s 
    -- (x += x) for any op, where x is being spilled (x is now in memory)
    | x == spillVarX && XL2S x == s = withNewVarRW $ \v -> [MathInst v op (XL2S v)]  
    -- (x += somethingElse) for any op, where x is being spilled (x is now in memory)
    | x == spillVarX = withNewVarRW $ \v -> [MathInst v op s]
    -- (y += x) for any op, where x is being spilled (x is now in memory)
    -- this updates y's value, so we don't have to write x back to memory
    | s == spillVarS = withNewVarR $ \v -> [MathInst x op (XL2S v)]
    -- spill var is not in the instruction, so just return it.
    | otherwise = return [MathInst x op s]

  spillMemWrite :: L2MemLoc -> L2S -> State Int [L2Instruction] 
  spillMemWrite loc@(MemLoc bp off) s
    -- ((mem x 4) <- x)
    -- (s_0 <- (mem ebp stackOffset))
    -- ((mem s_0 4) <- s_0)
    | bp == spillVarX && s == spillVarS = withNewVarR $ \v -> mw v (XL2S v)
    -- ((mem x 4) <- y)
    -- (s_0 <- (mem ebp stackOffset))
    -- ((mem s_0 4) <- y)
    | bp == spillVarX = withNewVarR $ \v -> mw v s
    -- ((mem y 4) <- x)
    -- (s_0 <- (mem ebp stackOffset))
    -- ((mem y 4) <- s_0)
    | s == spillVarS = withNewVarR $ \v -> mw bp (XL2S v)
    | otherwise = return $ [MemWrite loc s] where
    mw base val = [MemWrite (MemLoc base off) val]
