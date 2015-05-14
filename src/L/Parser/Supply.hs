module L.Parser.Supply
 ( Supply(..)
 , freshName
 , freshNameFor
 , freshNameFor'
 , member
 , newSupply
 , take
 ) where

import Control.Monad.State
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as Nel
import Data.Set (Set)
import qualified Data.Set as Set
import L.Variable
import Prelude hiding (take)

-- | A Supply supplies fresh variable names, unused in a particular program.
data Supply = Supply {
   well    :: NonEmpty Variable -- | An infinite stream of new names (some might be in members)
 , members :: Set Variable      -- | A Set of Variable names already in use.
} deriving Show

-- | Find a name for the given variable.
-- |  * If it isn't in the Supply, add it to the supply, and return both.
-- |  * If it isn't in the Supply, append the int 0 to it,
-- |    recur, and keep incrementing the int until we find an unused name.
-- |    Return the new Variable (with the int attached) and the new Supply
-- |    The new Supply will contain the new Variable
freshNameFor :: Variable -> State Supply Variable
freshNameFor v = state (freshNameForPure v)

-- | Quick helper for freshNameFor
freshNameFor' :: String -> State Supply Variable
freshNameFor' = freshNameFor . Variable

-- | Pulls Variables from the well until an unused name is found.
-- | Returns that Variable, and the updated Supply.
freshName :: State Supply Variable
freshName = state freshNamePure

-- | Takes n fresh variables from the supply, and updates the supply accordingly.
take :: Int -> State Supply [Variable]
take n = state (takePure n)

-- | Ask if the given Variable is in the Supply (and therefore some program)
member :: Variable -> Supply -> Bool
member v s = v `Set.member` (members s)

-- | Create a new supply of names, never generating any from the given set.
newSupply :: Set Variable -> Supply
newSupply s = Supply (Nel.fromList infiniteVars) s where
  chars = ['a'..'z']
  infiniteVars :: [Variable]
  infiniteVars = fmap Variable $ chars : f (0::Int) where f n = fmap (: show n) chars ++ f (n+1)
--(Nel.fromList . filter (not . flip Set.member s) infiniteNames)

-- | Find a name for the given variable.
freshNameForPure :: Variable -> Supply -> (Variable, Supply)
freshNameForPure v s = f 0 s where
  append :: Variable -> Int -> Variable
  append (Variable v) n = Variable (v ++ show n)
  f :: Int -> Supply -> (Variable, Supply)
  f n s | v' `member` s = f (n+1) s
        | otherwise = (v', insertUnsafe v' s)
    where v' =  v `append` n

-- | Pulls Variables from the well until an unused name is found.
-- | Returns that Variable, and the updated Supply.
freshNamePure :: Supply -> (Variable, Supply)
freshNamePure s@(Supply (n:|ns) usedSet)
  | member n s = freshNamePure $ Supply (Nel.fromList ns) usedSet
  | otherwise  = (n, Supply (Nel.fromList ns) (n `Set.insert` usedSet))

-- | Takes n fresh variables from the supply, and updates the supply accordingly.
takePure :: Int -> Supply -> ([Variable], Supply)
takePure n s = f n [] s where
  f 0 vs s = (reverse vs, s)
  f n vs s = let (v, s') = freshNamePure s in f (n-1) (v:vs) s'

-- | Insert the given Variable into the Supply
insertUnsafe :: Variable -> Supply -> Supply
insertUnsafe v (Supply vs us) = Supply vs (v `Set.insert` us)
