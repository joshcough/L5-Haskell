{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module L.L2.Vars
  (
    HasVars(..),
    isVariable
   ,replaceVarsWithRegisters
  ) where

import qualified Data.Map as M
import qualified Data.Set as S
import L.L1L2AST
 
isVariable :: L2X -> Bool
isVariable (VarL2X _) = True
isVariable _ = False

class HasVars a where
  vars :: a -> S.Set Variable

instance HasVars (L2Instruction a) where
  vars = varsInst

varsInst :: L2Instruction a -> S.Set Variable
varsInst = varsI where
  varsI (Assign x rhs)             = S.unions [varsX x,  varsRHS rhs]
  varsI (MathInst x _ s)           = S.unions [varsX x,  varsS s]
  varsI (MemWrite (MemLoc bp _) s) = S.unions [varsX bp, varsS s]
  varsI (Goto _)                   = S.empty
  varsI (CJump (Comp s1 _ s2) _ _) = S.unions [varsS s1, varsS s2]
  varsI (LabelDeclaration _)       = S.empty
  varsI (Call s)                   = varsS s
  varsI (TailCall s)               = varsS s
  varsI Return                     = S.empty

  varsX (RegL2X _) = S.empty
  varsX (VarL2X v) = S.singleton v

  varsS (XL2S x)        = varsX x
  varsS (NumberL2S n)   = S.empty
  varsS (LabelL2S l)    = S.empty

  varsRHS (CompRHS (Comp s1 _ s2)) = S.unions [varsS s1, varsS s2] 
  varsRHS (Allocate s1 s2)         = S.unions [varsS s1, varsS s2] 
  varsRHS (Print s)                = varsS s
  varsRHS (ArrayError a n)         = S.unions [varsS a,  varsS n]
  varsRHS (MemRead (MemLoc bp _))  = varsX bp
  varsRHS (SRHS s)                 = varsS s

-- replaces variables with registers in an L2 function.
replaceVarsWithRegisters :: M.Map Variable Register -> L2Func intsize -> L1Func intsize
replaceVarsWithRegisters replacements func = Func $ fmap replaceInInst (body func) where
  replaceInInst :: L2Instruction intsize -> L1Instruction intsize
  replaceInInst (Assign x rhs)        = Assign (getRegister x) (replaceInRHS rhs)
  replaceInInst (MathInst x op s)     = MathInst (getRegister x) op (replaceInS s)
  replaceInInst (MemWrite   loc s)    = MemWrite (replaceInMemLoc loc) (replaceInS s)
  replaceInInst (Goto s)              = Goto s
  replaceInInst (CJump comp l1 l2)    = CJump (replaceInComp comp) l1 l2
  replaceInInst (Call s)              = Call $ replaceInS s
  replaceInInst (TailCall s)          = TailCall $ replaceInS s
  replaceInInst (LabelDeclaration ld) = LabelDeclaration ld
  replaceInInst Return                = Return

  replaceInS :: L2S intsize -> L1S intsize
  replaceInS (XL2S (VarL2X v)) = maybe (error "bad register") RegL1S $ M.lookup v replacements
  replaceInS (XL2S (RegL2X r)) = RegL1S r
  replaceInS (NumberL2S n)     = NumberL1S n
  replaceInS (LabelL2S n)      = LabelL1S n

  replaceInRHS :: AssignRHS L2X (L2S intsize) -> AssignRHS L1X (L1S intsize)
  replaceInRHS (Allocate s1 s2)       = Allocate (replaceInS s1) (replaceInS s2)
  replaceInRHS (Print s)              = Print (replaceInS s)
  replaceInRHS (ArrayError s1 s2)     = ArrayError (replaceInS s1) (replaceInS s2)
  replaceInRHS (MemRead loc)          = MemRead (replaceInMemLoc loc)
  replaceInRHS (SRHS s)               = SRHS (replaceInS s)
  replaceInRHS (CompRHS comp)         = CompRHS (replaceInComp comp)

  replaceInMemLoc (MemLoc x off) = MemLoc (getRegister x) off
  replaceInComp (Comp s1 op s2)  = Comp   (replaceInS s1) op (replaceInS s2)

  getRegister :: L2X -> Register
  getRegister (VarL2X v) = maybe (error "bad register") id $ M.lookup v replacements
  getRegister (RegL2X r) = r
