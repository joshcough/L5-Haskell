module L.Variable where

import Control.Lens
import L.SExpr

newtype Variable = Variable String deriving (Eq, Ord, Read, Show)

instance AsSExpr Variable where
  asSExpr (Variable v) = AtomSym v

instance FromSExpr Variable where
  fromSExpr (AtomSym s) = Right $ Variable s
  fromSExpr bad = Left $ "invalid variable name: " ++ show bad

class AsVariable t where
  _Variable :: Prism' t Variable

instance AsVariable Variable where
  _Variable = id