module L.Supply where

import Control.Monad.State
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as Nel
import Data.Set (Set)
import qualified Data.Set as Set
import L.Variable

data Supply = Supply (NonEmpty Variable) (Set Variable) deriving Show

-- | Supply a (potentially new) name for the given variable
supplyNameM :: Variable -> State Supply Variable
supplyNameM v = do
  s <- get
  let (v', s') = f v s
  put s'
  return v' where
  f v (Supply (n:|ns) usedSet)
    | v `Set.member` usedSet = (n, Supply (Nel.fromList ns) (Set.insert n usedSet))
    | otherwise              = (v, Supply (n:|ns)           (Set.insert v usedSet))

supplyNameM' :: String -> State Supply Variable
supplyNameM' = supplyNameM . Variable

-- | create a new supply of names, never generating any from the given set.
newSupply :: Set Variable -> Supply
newSupply s = Supply vs Set.empty where
  vs = Nel.fromList . filter (not . flip Set.member s) $ [Variable [v] | v <- ['a'..'z']]