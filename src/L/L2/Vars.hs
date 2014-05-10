{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module L.L2.Vars
  (
    HasVars(..),
    isVariable
  ) where

import Data.Set (Set)
import qualified Data.Set as Set
import L.L1L2AST
 
isVariable :: L2X -> Bool
isVariable (VarL2X _) = True
isVariable _ = False

class HasVars a where
  vars :: a -> Set Variable

instance HasVars L2Instruction where
  vars = varsInst

varsInst :: L2Instruction -> Set Variable
varsInst = varsI where
  varsI (Assign x rhs)             = Set.unions [varsX x,  varsRHS rhs]
  varsI (MathInst x _ s)           = Set.unions [varsX x,  varsS s]
  varsI (MemWrite (MemLoc bp _) s) = Set.unions [varsX bp, varsS s]
  varsI (Goto _)                   = Set.empty
  varsI (CJump (Comp s1 _ s2) _ _) = Set.unions [varsS s1, varsS s2]
  varsI (LabelDeclaration _)       = Set.empty
  varsI (Call s)                   = Set.empty
  varsI (TailCall s)               = Set.empty
  varsI Return                     = Set.empty

  varsX (RegL2X _) = Set.empty
  varsX (VarL2X v) = Set.singleton v

  varsS (XL2S x)        = varsX x
  varsS (NumberL2S n)   = Set.empty
  varsS (LabelL2S l)    = Set.empty

  varsRHS (CompRHS (Comp s1 _ s2)) = Set.unions [varsS s1, varsS s2] 
  varsRHS (Allocate s1 s2)         = Set.unions [varsS s1, varsS s2] 
  varsRHS (Print s)                = varsS s
  varsRHS (ArrayError a n)         = Set.unions [varsS a,  varsS n]
  varsRHS (MemRead (MemLoc bp _))  = varsX bp
  varsRHS (SRHS s)                 = varsS s
