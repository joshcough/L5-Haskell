module L.L4.ANormalize (aNormalize) where

import Control.Applicative
import Control.Monad.State
import Data.Traversable
import L.L1L2AST (Variable, Label)
import L.L3.L3AST as L3
import L.L4.L4AST as L4

writeMe = error "todo"

newVar :: State Int Variable
newVar = incState "__tempL3V"
newLabel :: State Int Label
newLabel = incState "__tempL3L"
incState :: String -> State Int String
incState prefix = do { n <- get; put (n + 1); return $ prefix ++ show n }

aNormalize :: L4 -> L3
aNormalize l4 = fst $ runState (aNormalizeS l4) 0

aNormalizeS :: L4 -> State Int L3
aNormalizeS (L4 e funcs) = writeMe

compileV :: L4.V -> L3.V
compileV (L4.VarV v)   = L3.VarV v
compileV (L4.NumV i)   = L3.NumV i
compileV (L4.LabelV l) = L3.LabelV l

