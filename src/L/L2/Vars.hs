{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module L.L2.Vars
  (
    HasVars(..),
    isVariable
  ) where

import Data.Monoid
import Data.Set (Set)
import qualified Data.Set as Set
import L.L1L2AST
import L.Variable
 
isVariable :: L2X -> Bool
isVariable (VarL2X _) = True
isVariable _ = False

class HasVars a where
  vars :: a -> Set Variable

instance HasVars L2Instruction where
  vars = varsInst

varsInst :: L2Instruction -> Set Variable
varsInst = varsI where
  varsI (Assign x rhs)             = varsX x  <> varsRHS rhs
  varsI (MathInst x _ s)           = varsX x  <>  varsS s
  varsI (MemWrite (MemLoc bp _) s) = varsX bp <> varsS s
  varsI (Goto _)                   = mempty
  varsI (CJump (Comp s1 _ s2) _ _) = varsS s1 <> varsS s2
  varsI (LabelDeclaration _)       = mempty
  varsI (Call s)                   = varsS s
  varsI (TailCall s)               = varsS s
  varsI Return                     = mempty

  varsX (RegL2X _) = mempty
  varsX (VarL2X v) = Set.singleton v

  varsS (XL2S x)        = varsX x
  varsS (NumberL2S _)   = mempty
  varsS (LabelL2S _)    = mempty

  varsRHS (CompRHS (Comp s1 _ s2)) = varsS s1 <> varsS s2
  varsRHS (Allocate s1 s2)         = varsS s1 <> varsS s2
  varsRHS (Print s)                = varsS s
  varsRHS (ArrayError a n)         = varsS a  <> varsS n
  varsRHS (MemRead (MemLoc bp _))  = varsX bp
  varsRHS (SRHS s)                 = varsS s
