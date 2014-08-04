module L.L4.ANormalize (aNormalize) where

import Control.Applicative
import Control.Monad.State
import Data.List
import Data.Traversable
import L.L1L2AST (Variable, Label)
import L.L3.L3AST as L3
import L.L4.L4AST as L4

writeMe = error "todo"

newVar :: State Int Variable
newVar = incState "__tempL4V"
newLabel :: State Int Label
newLabel = incState "__tempL4L"
incState :: String -> State Int String
incState prefix = do { n <- get; put (n + 1); return $ prefix ++ show n }

type Keyword = String

data Context = 
    LetContext Variable L4.E Context 
  | IfContext  L4.E L4.E Context 
  | FunCallContext [L4.E] (Maybe Keyword) [L3.V] Context
  | NoContext

aNormalize :: L4 -> L3
aNormalize l4 = fst $ runState (aNormalizeS l4) 0

aNormalizeS :: L4 -> State Int L3
aNormalizeS (L4 main funcs) = do
  (main' , mainFs)    <- l4Find' main
  (funcs', moreFuncs) <- unzip <$> (traverse l4FindF funcs)
  return $ L3 main' $ mainFs ++ funcs' ++ concat moreFuncs

l4FindF :: L4.Func -> State Int (L3.Func, [L3.Func])
l4FindF (L4.Func name args body) = do
  (fBody, extra) <- l4Find body NoContext
  return (L3.Func name args fBody, extra)

l4Find' :: L4.E -> State Int (L3.E, [L3.Func])
l4Find' e =  l4Find e NoContext

l4Find :: L4.E -> Context -> State Int (L3.E, [L3.Func])
l4Find (L4.Let v e body)      k = l4Find e $ LetContext v body k
l4Find (L4.IfStatement c t f) k = l4Find c $ IfContext t f k
l4Find (Begin e1 e2)          k = do { v <- newVar; l4Find (L4.Let v e1 e2) k }

{-
  case FunCall(kw:Keyword, Nil) => fill(kw.toD(Nil), k) // (new-tuple) case
  case FunCall(kw:Keyword, e::es) => find(e, FunCallContext(es, Some(kw), Nil, k))
  case FunCall(e:E, es) => find(e, FunCallContext(es, None, Nil, k))
  case v:V => fill(convertV(v), k)
-}


fill :: L3.D -> Context -> State Int (L3.E, [L3.Func]) 
fill d = fill' where
  fill' NoContext = return (DE d, [])
  fill' (LetContext v b k) = do
      (letBody, extraFuncs) <- l4Find b k
      return (L3.Let v d letBody, extraFuncs)


compileV :: L4.V -> L3.V
compileV (L4.VarV v)   = L3.VarV v
compileV (L4.NumV i)   = L3.NumV i
compileV (L4.LabelV l) = L3.LabelV l

freeVars :: L3.E -> [Variable] -> [Variable]
freeVars e boundVars = nub $ f e boundVars where
  ve = DE . VD
  f :: L3.E -> [Variable] -> [Variable]
  f (L3.Let x r body)              bv = f (DE r) bv ++ f body (x : bv)
  f (L3.IfStatement e a b)         bv = f (ve e) bv ++ f a bv ++  f b bv
  f (DE (VD (L3.VarV v)))          bv = maybe [] (:[]) $ Data.List.find (v==) bv
  f (DE (VD _))                    bv = []
  f (DE (BiopD (L3.Add l r)))      bv = f (ve l) bv ++ f (ve r) bv
  f (DE (BiopD (L3.Sub l r)))      bv = f (ve l) bv ++ f (ve r) bv
  f (DE (BiopD (L3.Mult l r)))     bv = f (ve l) bv ++ f (ve r) bv
  f (DE (BiopD (L3.LessThan l r))) bv = f (ve l) bv ++ f (ve r) bv
  f (DE (BiopD (L3.LTorEq l r)))   bv = f (ve l) bv ++ f (ve r) bv
  f (DE (BiopD (L3.Eq l r)))       bv = f (ve l) bv
  f (DE (PredD (L3.IsNum l)))      bv = f (ve l) bv
  f (DE (PredD (L3.IsArray l)))    bv = f (ve l) bv
  f (DE (L3.FunCall v vs))         bv = f (ve v) bv ++ (ve <$> vs >>= flip f bv)
  f (DE (L3.NewArray size init))   bv = f (ve size) bv ++ f (ve init) bv
  f (DE (L3.NewTuple vs))          bv = ve <$> vs >>= flip f bv
  f (DE (L3.ARef arr loc))         bv = f (ve arr) bv ++ f (ve loc) bv
  f (DE (L3.ASet arr loc v))       bv = f (ve arr) bv ++ f (ve loc) bv ++ f (ve v) bv
  f (DE (L3.ALen arr))             bv = f (ve arr) bv
  f (DE (L3.MakeClosure _ v))      bv = f (ve v) bv
  f (DE (L3.ClosureProc v))        bv = f (ve v) bv
  f (DE (L3.ClosureVars v))        bv = f (ve v) bv
  f (DE (L3.Print v))              bv = f (ve v) bv
