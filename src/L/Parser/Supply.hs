module L.Parser.Supply
 (
  Supply(..)
, freshNameForS
, freshNameForS'
, newSupply
 ) where

import Control.Monad.State
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as Nel
import Data.Set (Set)
import qualified Data.Set as Set
import L.Variable
import Prelude hiding (take)

data Supply = Supply (NonEmpty Variable) (Set Variable) deriving Show

-- | Supply a (potentially new) name for the given variable
freshNameForS :: Variable -> State Supply Variable
freshNameForS v = state (freshNameFor v)

freshNameForS' :: String -> State Supply Variable
freshNameForS' = freshNameForS . Variable

takeS :: Int -> State Supply [Variable]
takeS n = state (take n)

take :: Int -> Supply -> ([Variable], Supply)
take n s = f n [] s where
  f 0 vs s = (reverse vs, s)
  f n vs s = let (v, s') = freshName s in f (n-1) (v:vs) s'

insert :: Variable -> Supply -> (Variable, Supply)
insert = freshNameFor

insertUnsafe :: Variable -> Supply -> Supply
insertUnsafe v (Supply vs us) = Supply vs (Set.insert v us)

-- TODO: use v to determine the next Variable to give.
freshNameFor :: Variable -> Supply -> (Variable, Supply)
freshNameFor v s = f 0 s where
  f n s | v `member` s = f (n+1) s
        | otherwise    = (v, insertUnsafe v s)

freshName :: Supply -> (Variable, Supply)
freshName s@(Supply (n:|ns) usedSet)
  | member n s = freshName $ Supply (Nel.fromList ns) usedSet
  | otherwise  = (n, Supply (Nel.fromList ns) (Set.insert n usedSet))

member :: Variable -> Supply -> Bool
member v (Supply _ u) = v `Set.member` u

-- | create a new supply of names, never generating any from the given set.
newSupply :: Set Variable -> Supply
newSupply s = Supply (Nel.fromList infiniteNames) Set.empty where
  chars = ['a'..'z']
  infiniteNames :: [Variable]
  infiniteNames = fmap Variable $
    chars : f 0 where f n = fmap (: show n) chars ++ f (n+1)
--(Nel.fromList . filter (not . flip Set.member s) infiniteNames)
