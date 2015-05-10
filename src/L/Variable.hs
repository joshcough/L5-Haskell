module L.Variable where

import Control.Lens
import L.Parser.SExpr
import Data.Set (Set)
import qualified Data.Set as Set

type Hint = Variable

newtype Variable = Variable String deriving (Eq, Ord, Read, Show)

instance AsSExpr Variable where
  asSExpr (Variable v) = AtomSym v

instance FromSExpr Variable where
  fromSExpr (AtomSym s) = return $ Variable s
  fromSExpr bad = fail $ "invalid variable name: " ++ show bad

class AsVariable t where
  _Variable :: Prism' t Variable

instance AsVariable Variable where
  _Variable = id

class FreeVars a where
  freeVars :: a -> Set Variable -> Set Variable

-- TODO: join all errors together?
wellFormedArgList :: [SExpr] -> Either String [Variable]
wellFormedArgList = traverse wellFormedArg where

wellFormedArg :: SExpr -> Either String Variable
wellFormedArg (AtomSym arg) = wellFormedArgString arg
wellFormedArg bad           = fail $ "invalid argument name: " ++ show bad  

-- TODO: test for valid variable name?
-- it might sort of be handled automatically in Read, but
-- still might be worth doing something here. consider it..
wellFormedArgString :: String -> Either String Variable
wellFormedArgString arg = return $ Variable arg