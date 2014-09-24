module L.L4.ANormalize (aNormalize) where

import Control.Applicative
import Control.Monad.State
import Data.List
import Data.Traversable
import L.L1L2AST (Variable, Label)
import L.L3.L3AST as L3
import L.L4.L4AST as L4

newVar :: State Int Variable
newVar = incState "__tempL4V"
newLabel :: State Int Label
newLabel = incState "__tempL4L"
incState :: String -> State Int String
incState prefix = do { n <- get; put (n + 1); return $ prefix ++ show n }

data Context = 
    LetContext Variable L4.E Context 
  | IfContext  L4.E L4.E Context 
  | FunCallContext [L4.E] (Maybe ([L3.V] -> L3.D)) [L3.V] Context
  | NoContext

aNormalize :: L4 -> L3
aNormalize l4 = fst $ runState (aNormalizeS l4) 0

aNormalizeS :: L4 -> State Int L3
aNormalizeS (L4 main funcs) = do
  (main' , mainFs)    <- l4Find main NoContext
  (funcs', moreFuncs) <- unzip <$> (traverse l4FindF funcs)
  return $ L3 main' $ mainFs ++ funcs' ++ concat moreFuncs

l4FindF :: L4.Func -> State Int (L3.Func, [L3.Func])
l4FindF (L4.Func name args body) = do
  (fBody, extra) <- l4Find body NoContext
  return (L3.Func name args fBody, extra)

l4Find :: L4.E -> Context -> State Int (L3.E, [L3.Func])
l4Find e c = go e c where 
  go (L4.Let v e body)       k = go e $ LetContext v body k
  go (L4.IfStatement c t f)  k = go c $ IfContext t f k
  go (L4.Begin e1 e2)        k = do { v <- newVar; go (L4.Let v e1 e2) k }
  go (L4.NewTuple [])        k = fill (L3.NewTuple []) k -- special case
  go (L4.FunCall f args)     k = go f  (FunCallContext args Nothing [] k)
  go (L4.NewArray e1 e2)     k = go e1 (FunCallContext [e2]    (jw2 L3.NewArray) [] k)
  go (L4.NewTuple (e1:es))   k = go e1 (FunCallContext es      (Just $ L3.NewTuple) [] k)
  go (L4.ARef e1 e2)         k = go e1 (FunCallContext [e2]    (jw2 L3.ARef)  [] k)
  go (L4.ASet e1 e2 e3)      k = go e1 (FunCallContext [e2,e3] (jw3 L3.ASet)  [] k)
  go (L4.ALen e1)            k = go e1 (FunCallContext []      (jw1 L3.ALen)  [] k)
  go (L4.Print e1)           k = go e1 (FunCallContext []      (jw1 L3.Print) [] k)
  go (L4.MakeClosure lbl e1) k = go e1 (FunCallContext []      (jw1 (L3.MakeClosure lbl)) [] k)
  go (L4.ClosureProc e1)     k = go e1 (FunCallContext []      (jw1 L3.ClosureProc) [] k)
  go (L4.ClosureVars e1)     k = go e1 (FunCallContext []      (jw1 L3.ClosureVars) [] k)
  go (L4.BiopE b e1 e2)      k = go e1 (FunCallContext [e2]    (jw2 (L3.BiopD b)) [] k)
  go (L4.PredE p e1)         k = go e1 (FunCallContext []      (jw1 (L3.PredD p)) [] k)
  go (VE v)                  k = fill (VD v) k

  jw1 f = Just (wrangle1 f)
  jw2 f = Just (wrangle2 f)
  jw3 f = Just (wrangle3 f)

  wrangle1 :: Show a => (a -> b) -> [a] -> b
  wrangle1 f (a:[]) = f a
  wrangle1 _ as = error $ "expected 1 arg, but got: " ++ show as

  wrangle2 :: Show a => (a -> a -> b) -> [a] -> b
  wrangle2 f (a1:a2:[]) = f a1 a2
  wrangle2 _ as = error $ "expected 2 args, but got: " ++ show as

  wrangle3 :: Show a => (a -> a -> a -> b) -> [a] -> b
  wrangle3 f (a1:a2:a3:[]) = f a1 a2 a3
  wrangle3 _ as = error $ "expected 3 args, but got: " ++ show as

-- TODO: doing some bad appending onto back of lists here...
-- should probably reverse the vs.
fill :: L3.D -> Context -> State Int (L3.E, [L3.Func]) 
fill d = fill' where
  fill' :: Context -> State Int (L3.E, [L3.Func])
  fill' NoContext = return (DE d, [])
  fill' (LetContext v b k) = do
    (letBody, extraFuncs) <- l4Find b k
    return (L3.Let v d letBody, extraFuncs)
  fill' (FunCallContext (e:es) makeD vs k) = 
    maybeLet d $ \v -> l4Find e $ FunCallContext es makeD (vs++[v]) k 
  fill' (FunCallContext [] (Just makeD) vs k) = 
    maybeLet d $ \v -> fill (makeD $ vs++[v]) k
  fill' (FunCallContext [] Nothing vs k) = maybeLet d $ \v -> 
    let vsv = vs ++ [v]
    in fill (L3.FunCall (head vsv) (tail vsv)) k
  {-
   - (+ (if v e_1 e_2) e_big) =>
   -   (let ((ctxt (lambda (ret-val) (+ ret-val e_big))))
   -     (if v (ctxt e_1) (ctxt e_2)))
   -}
  fill' (IfContext t e k) = maybeLet d $ \v -> do
    fLabel <- newLabel
    fArg   <- newVar
    (fBody, extraFuncsFromFBody) <- fill (VD $ VarV fArg) k
    let frees = filter (/=fArg) (freeVars fBody)
        freesTup = "frees"
        fBodyWithFrees = foldr f fBody (zip frees [0..]) where
          f (v,i) b = L3.Let v (L3.ARef (VarV freesTup) (NumV i)) b
        func = L3.Func fLabel [fArg, freesTup] fBodyWithFrees
        tup  = L4.NewTuple (VE . VarV <$> frees)
    (tt, tef) <- l4Find (L4.FunCall (VE $ LabelV fLabel) [t, tup]) NoContext
    (ee, eef) <- l4Find (L4.FunCall (VE $ LabelV fLabel) [e, tup]) NoContext
    return (L3.IfStatement v tt ee, concat [[func], extraFuncsFromFBody, tef, eef])

  maybeLet :: L3.D -> (V -> State Int (L3.E, [L3.Func])) -> State Int (L3.E, [L3.Func])
  maybeLet (VD v) f = f v
  maybeLet d      f = do
    x                    <- newVar
    (letBody, extraFuns) <- f $ VarV x
    return (L3.Let x d letBody, extraFuns) 

freeVars :: L3.E -> [Variable]
freeVars e = nub $ f e [] where
  ve = DE . VD
  f :: L3.E -> [Variable] -> [Variable]
  f (L3.Let x r body)            bv = f (DE r) bv ++ f body (x : bv)
  f (L3.IfStatement e a b)       bv = f (ve e) bv ++ f a bv ++  f b bv
  f (DE (VD (L3.VarV v)))        bv = maybe [] (:[]) $ Data.List.find (v==) bv
  f (DE (VD _))                  bv = []
  f (DE (BiopD _ l r))           bv = f (ve l) bv ++ f (ve r) bv
  f (DE (PredD _ l))             bv = f (ve l) bv
  f (DE (L3.FunCall v vs))       bv = f (ve v) bv ++ (ve <$> vs >>= flip f bv)
  f (DE (L3.NewArray size init)) bv = f (ve size) bv ++ f (ve init) bv
  f (DE (L3.NewTuple vs))        bv = ve <$> vs >>= flip f bv
  f (DE (L3.ARef arr loc))       bv = f (ve arr) bv ++ f (ve loc) bv
  f (DE (L3.ASet arr loc v))     bv = f (ve arr) bv ++ f (ve loc) bv ++ f (ve v) bv
  f (DE (L3.ALen arr))           bv = f (ve arr) bv
  f (DE (L3.MakeClosure _ v))    bv = f (ve v) bv
  f (DE (L3.ClosureProc v))      bv = f (ve v) bv
  f (DE (L3.ClosureVars v))      bv = f (ve v) bv
  f (DE (L3.Print v))            bv = f (ve v) bv

