module L.SExpr 
  (
  	SExpr(..)
  , AsSExpr(..)
  , FromSExpr(..)
  , list
  , num
  , showAsList
  , showSExpr
  , sym
  ) where

import Data.Int
import L.Utils (mkString)

data SExpr = AtomSym String | AtomNum Int64 | List [SExpr] deriving (Eq)

sym :: String -> SExpr
sym = AtomSym

num :: Int64 -> SExpr
num = AtomNum

list :: AsSExpr a => [a] -> SExpr
list = List . fmap asSExpr

instance Show SExpr where
  show (AtomSym s) = s
  show (AtomNum i) = show i
  show (List exps) = showAsList $ fmap show exps

showSExpr :: AsSExpr a => a -> String
showSExpr = show . asSExpr

class AsSExpr a where
  asSExpr :: a -> SExpr

instance AsSExpr Int64 where
  asSExpr = AtomNum

instance AsSExpr SExpr where
  asSExpr = id

instance AsSExpr a => AsSExpr [a] where 
  asSExpr as = List $ fmap asSExpr as

instance (AsSExpr a, AsSExpr b) => AsSExpr (a,b) where 
  asSExpr (a,b) = List $ [asSExpr a, asSExpr b]

instance (AsSExpr a, AsSExpr b, AsSExpr c) => AsSExpr (a,b,c) where 
  asSExpr (a,b,c) = List $ [asSExpr a, asSExpr b, asSExpr c]

instance (AsSExpr a, AsSExpr b, AsSExpr c, AsSExpr d) => AsSExpr (a,b,c,d) where 
  asSExpr (a,b,c,d) = List $ [asSExpr a, asSExpr b, asSExpr c, asSExpr d]

instance (AsSExpr a, AsSExpr b, AsSExpr c, AsSExpr d, AsSExpr e) => AsSExpr (a,b,c,d,e) where 
  asSExpr (a,b,c,d,e) = List $ [asSExpr a, asSExpr b, asSExpr c, asSExpr d, asSExpr e]

instance (AsSExpr a, AsSExpr b, AsSExpr c, AsSExpr d, AsSExpr e, AsSExpr f) => AsSExpr (a,b,c,d,e,f) where 
  asSExpr (a,b,c,d,e,f) = List $ [asSExpr a, asSExpr b, asSExpr c, asSExpr d, asSExpr e, asSExpr f]

class FromSExpr a where
  fromSExpr :: SExpr -> Either String a 

instance (FromSExpr a) => FromSExpr [a] where
  fromSExpr (List args) = sequence (fmap fromSExpr args)
  fromSExpr bad         = Left $ "bad list" ++ show bad 

showAsList :: [String] -> String
showAsList as = "(" ++ (mkString " " as) ++ ")"

