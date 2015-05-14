module L.L4.ANormalize (aNormalize) where

import Control.Applicative
import Control.Monad
import Control.Monad.State
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import qualified Data.Set as Set
import Data.Traversable
import L.Parser.Supply
import L.Primitives (Label(..))
import L.L3.L3AST as L3
import L.L4.L4AST as L4
import L.Variable

freshenString :: String -> State Int String
freshenString = incState
freshenVar :: Variable -> State Int Variable
freshenVar (Variable v) = Variable <$> freshenString v
newVar :: State Int Variable
newVar = freshenVar $ Variable "_l4_"
newLabel :: State Int Label
newLabel = Label <$> freshenString ":l4_"
incState :: String -> State Int String
incState prefix = do { n <- get; put (n + 1); return $ prefix ++ "_" ++  show n }

data Context = 
    LetContext Variable L4.E Context
  | IfContext  L4.E L4.E Context
  | FunCallContext [L4.E] (Maybe ([L3.V] -> L3.D)) [L3.V] Context
  | NoContext

aNormalize :: L4 -> L3
aNormalize l4 = fst $ runState (aNormalizeS l4) 0

aNormalizeS :: L4 -> State Int L3
aNormalizeS p = do
  (L4 main funcs)     <- freshenL4 p
  (main' , mainFs)    <- l4Find main NoContext
  (funcs', moreFuncs) <- unzip <$> (traverse l4FindF funcs)
  return $ L3 main' $ mainFs ++ funcs' ++ concat moreFuncs

l4FindF :: L4.L4Func -> State Int (L3.L3Func, [L3.L3Func])
l4FindF (Func name args body) = do
  (fBody, extra) <- l4Find body NoContext
  return (Func name args fBody, extra)

l4Find :: L4.E -> Context -> State Int (L3.E, [L3.L3Func])
l4Find e c = go e c where
  go (L4.Let x r body)      k = go r $ LetContext x body k
  go (L4.If  c t f)         k = go c $ IfContext t f k
  go (L4.Begin e1 e2)       k = do { v <- newVar; go (L4.Let v e1 e2) k }
  go (L4.NewTuple [])       k = fill  (L3.NewTuple []) k -- special case
  go (L4.App f args)        k = go f  (FunCallContext args Nothing [] k)
  go (L4.PrimApp p (e:es))  k = go e  (FunCallContext es (Just $ wranglePrim p)  [] k)
  go (L4.PrimApp p [])      _ = fail $ show p ++ " given 0 arguments"
  go (L4.NewTuple (e1:es))  k = go e1 (FunCallContext es (Just $ L3.NewTuple) [] k)
  go (L4.MakeClosure l e1)  k = go e1 (FunCallContext [] (Just $ wrangle1 (L3.MakeClosure l)) [] k)
  go (L4.ClosureProc e1)    k = go e1 (FunCallContext [] (Just $ wrangle1  L3.ClosureProc)    [] k)
  go (L4.ClosureVars e1)    k = go e1 (FunCallContext [] (Just $ wrangle1  L3.ClosureVars)    [] k)
  go (VE v)                 k = fill (VD v) k

  wranglePrim :: PrimName -> [V] -> D
  wranglePrim pn = f (arityByName pn) where
    f 1 [v]        = L3.PrimApp pn [v]
    f 2 [v1,v2]    = L3.PrimApp pn [v1,v2]
    f 3 [v1,v2,v3] = L3.PrimApp pn [v1,v2,v3]
    f n args = error $ "expected " ++ show n ++ " arg(s), but got: " ++ show args

  wrangle1 :: Show a => (a -> b) -> [a] -> b
  wrangle1 f (a:[]) = f a
  wrangle1 _ as = error $ "expected 1 arg, but got: " ++ show as

-- TODO: doing some bad appending onto back of lists here...
-- should probably reverse the vs.
fill :: L3.D -> Context -> State Int (L3.E, [L3.L3Func]) 
fill d = fill' where
  fill' :: Context -> State Int (L3.E, [L3.L3Func])
  fill' NoContext = return (DE d, [])
  fill' (LetContext x b k) = do
    (letBody, extraFuncs) <- l4Find b k
    return (L3.Let x d letBody, extraFuncs)
  fill' (FunCallContext (e:es) makeD vs k) = 
    maybeLet d $ \v -> l4Find e $ FunCallContext es makeD (vs++[v]) k 
  fill' (FunCallContext [] (Just makeD) vs k) = 
    maybeLet d $ \v -> fill (makeD $ vs++[v]) k
  fill' (FunCallContext [] Nothing vs k) = maybeLet d $ \v ->
    let vsv = vs ++ [v] in fill (L3.App (head vsv) (tail vsv)) k
  {-
   - (+ (if v e_1 e_2) e_big) =>
   -   (let ((ctxt (lambda (ret-val) (+ ret-val e_big))))
   -     (if v (ctxt e_1) (ctxt e_2)))
   -}
  fill' (IfContext t e k) = maybeLet d $ \v -> do
    fLabel <- newLabel
    fArg   <- newVar
    (fBody, extraFuncsFromFBody) <- fill (VD $ VarV fArg) k
    let frees = Set.toList $ Set.filter (/=fArg) (freeVars fBody Set.empty)
        freesTup = Variable "frees"
        fBodyWithFrees = foldr f fBody (zip frees [0..]) where
          f (v,i) b = L3.Let v (L3.PrimApp ARef [VarV freesTup, NumV i]) b
        func = Func fLabel [fArg, freesTup] fBodyWithFrees
        tup  = L4.NewTuple (VE . VarV <$> frees)
    (tt, tef) <- l4Find (L4.App (VE $ LabelV fLabel) [t, tup]) NoContext
    (ee, eef) <- l4Find (L4.App (VE $ LabelV fLabel) [e, tup]) NoContext
    return (L3.If v tt ee, concat [[func], extraFuncsFromFBody, tef, eef])

  maybeLet :: L3.D -> (V -> State Int (L3.E, [L3.L3Func])) -> State Int (L3.E, [L3.L3Func])
  maybeLet (VD v) f = f v
  maybeLet d      f = do
    x                    <- newVar
    (letBody, extraFuns) <- f $ VarV x
    return (L3.Let x d letBody, extraFuns) 

{- TODO: It's very likely that with the Supply stuff, this can all just go away... -}
freshenL4 :: L4 -> State Int L4
freshenL4 (L4 e fs) = L4 <$> freshenE e <*> traverse freshenFunc fs where
  freshenE :: L4.E -> State Int L4.E
  freshenE e = go e Map.empty
  freshenFunc (Func name args body) = do
    freshArgs <- traverse freshenVar args
    Func name freshArgs <$> go body (Map.fromList $ zip args freshArgs)
  go :: L4.E -> Map Variable Variable -> State Int L4.E
  go (L4.Let x r body)      m =
    do { v <- freshenVar x; L4.Let v <$> go r m <*> go body (Map.insert x v m) }
  go (L4.If c t f)        m = L4.If            <$> go c m  <*> go t m <*> go f m
  go (L4.Begin e1 e2)     m = L4.Begin         <$> go e1 m <*> go e2 m
  go (L4.NewTuple es)     m = L4.NewTuple      <$> traverse (flip go m) es
  go (L4.App f args)      m = L4.App           <$> go f m  <*> traverse (flip go m) args
  go (L4.PrimApp p es)    m = L4.PrimApp p     <$> traverse (flip go m) es
  go (L4.MakeClosure l e) m = L4.MakeClosure l <$> go e m
  go (L4.ClosureProc e)   m = L4.ClosureProc   <$> go e m
  go (L4.ClosureVars e)   m = L4.ClosureVars   <$> go e m
  go (VE (VarV v))        m = return . VE . VarV $ fromMaybe v (Map.lookup v m)
  go ve@(VE _)            _ = return ve
