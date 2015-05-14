{-# LANGUAGE TypeFamilies #-}

module L.L5.LambdaLifter (lambdaLift) where

import Control.Applicative
import Control.Monad.State
import Data.Monoid
import qualified Data.Set as Set
import Data.Traversable
import L.Primitives
import L.L3.L3AST (V(..))
import L.L4.L4AST as L4
import L.L5.L5AST as L5
import L.Parser.SExpr
import L.Parser.Supply
import L.Primitives (Label(..))
import L.Variable hiding (freeVars)

lambdaLift :: L5 -> L4
lambdaLift l5 = fst $ runState (go l5) (newSupply mempty)

{-
L5Compiler is a lambda lifter (http://en.wikipedia.org/wiki/Lambda_lifting).

L5 is the language being compiled. It contains one top level expression only.
The grammar for L5 can be found in the same directory as this file, L5-Grammar.txt.

The L5 compiler produces L4 expressions. One top expression ('main') and
a list of L4 procedures. These procedures are the lambdas in the L5 expression
lifted out to be top level procedures.

The L4 grammar can be found in the L4Compiler directory, a sibling of
the L5Compiler directory where this file (L5Compiler.scala) lives.
-}

{-
The main compile function.

The function works by matching on the type of e,
one case for each type of e (from the grammar).

The first several cases, Num, Var, If, Begin, NewTuple, and Let
are all trivial cases, transforming directly to the same L4 version.
The remainder of the cases are each documented in place.

@param e - the L5 expression to compile
@return - a tuple containing one top level L4 expression and a list of L4 functions
         the functions are the lifted lambdas.
-}
l5c :: String -> L4
l5c s = fst $ runState (go l5e) (newSupply mempty) where
  l5e = (either error id) . fromSExpr $ sread s

go :: L5.E -> State Supply L4
go (LitInt i)         = return $ L4 (VE . NumV $ fromIntegral i) []
go (Var v)            = return $ L4 (VE . VarV $ v) []
go (L5.If e t g)      = do
  (L4 e' efs) <- go e
  (L4 t' tfs) <- go t
  (L4 g' gfs) <- go g
  return $ L4 (L4.If e' t' g') (efs++tfs++gfs)
go (L5.Begin e1 e2)   = do
  (L4 e1' e1fs) <- go e1
  (L4 e2' e2fs) <- go e2
  return $ L4 (L4.Begin e1' e2') (e1fs++e2fs)
go (L5.NewTuple es)   = do
  (l4es, fs) <- goEs es
  return $ L4 (L4.NewTuple l4es) (concat fs)
go (L5.Let v e b)     = do
  v'          <- freshNameFor v
  (L4 e' efs) <- go e
  (L4 b' bfs) <- go b
  return $ L4 (L4.Let v' e' b') (efs++bfs)
{-
 letrec doesn't exist in L4, so we do this simple transformation.
 (letrec ([x e1]) e2)
  =>
 (let ([x (new-tuple 0)])
   (begin (aset x 0 e1[x:=(aref x 0)])
          e2[x:=(aref x 0)]))
-}
go (LetRec v e b) = go $
  L5.Let v (L5.NewTuple [LitInt 0]) (L5.Begin
    (L5.App (PrimE ASet) [Var v, LitInt 0, subst v (L5.App (PrimE ARef) [Var v, LitInt 0]) e])
    (subst v (L5.App (PrimE ARef) [Var v, LitInt 0]) b)
  )
{-
 We Turn (f +) => (f (lambda (x y) (+ x y)))
 So when we see a primitive function by itself,
 turn it into a lambda expression, and then compile that expression.
 -}
go pe@(PrimE p) = go $ Lambda (primVars p) (L5.App pe (Var <$> primVars p))
{-
 Primitive function application
 (+ x y z)  => (+ x y z)
 also a trivial case, but I wanted to keep this case close to the other App case.
-}
go (L5.App (PrimE p) es) = do
  (es', fs) <- goEs es
  return $ L4 (L4.PrimApp p es') (concat fs)
go (L5.App e es) = do
  v' <- freshNameFor' "x"
  -- compile the function position
  (L4 e' fs') <- go e
  -- compile all of the arguments
  (es', fs'') <- goEs es
  let ve = VE $ VarV v'
  return $ L4 (L4.Let v' e' (
    -- free variables go in the first argument.
    L4.App (L4.ClosureProc ve) $
      -- if we can fit the rest of the arguments in, then great
      -- if not, they must also go into another tuple.
      L4.ClosureVars ve : if length es' <= 2 then es' else [L4.NewTuple es']
   )) (concat $ fs' : fs'')
{-
 Here is where we actually do the lambda lifting.

 The general tranformation looks a bit like this:

  (lambda (x ...) e) => (make-closure :f (new-tuple y1 y2 ... y-n))
  where (y1 y2 ... y-n) are the free variables in (lambda (x ...) e),
  and we create a new procedure:
  (:f (vars-tup x ...)
    (let ([y1 (aref vars-tup 0)])
      (let ([y2 (aref vars-tup 1)])
        ...
          (let ([y-n (aref vars-tup n)])
             e))))

 At the application site, we replace the lambda with a closure.
 The closure contains the name of the new top level function
 and all of the free variables in the body of the lambda.

 The body of the lambda gets yanked out into the top level function.

 The first argument of the new function is the tuple (array)
 containing the values of the free variables in the original lambda.

 L4 functions can only have three arguments. So we may need to do one
 more transformation. If the lambda itself had two or fewer formal args,
 then we can simply fit them in. But if it had 3 or more, we can't fit
 them in (because we have one extra argument now for the free vars)
 In this case, we create a new tuple for them as well and so the function
 will take two tuple arguments, one for the free vars and one for
 the remainder of the arguments.

 For the free vars tuple, we have to transform the body of the function
 to first get the free vars out of the tuple, using the same name name
 as the original argument. For example if we have (lambda (x) y)
 then the first thing we do in the new function is (let ([y (aref vars-tup 0)])
 One let each of the free vars.

 If we need to use a tuple for the remaining arguments, we create lets
 in the very same way.
 -}
go lam@(Lambda args body) = do
  {- build the body of the new function -}
  -- compile the body of the lambda
  (L4 compiledLambdaBody moreFunctions) <- go body
  let usingArgsTuple = length args > 2
      freesVar = Variable "frees"
      argsVar  = Variable "args"
      -- the arguments to the new function
      fArgs = if usingArgsTuple then [freesVar, argsVar] else freesVar : args
      -- the free variables in the lambda
      -- TODO: make this into a set?
      frees = freeVars lam
      -- then wrap it with the required let statements
      freeLets = wrapWithLets freesVar (error "todo:158" frees) compiledLambdaBody
      liftedFunctionBody = if   usingArgsTuple 
                           then wrapWithLets argsVar args freeLets 
                           else freeLets
  liftedLabel <- fmap toLabel (freshNameFor' "x")
  let closure = L4.MakeClosure liftedLabel $ L4.NewTuple (VE . VarV <$> (error "todo:163" frees))
      liftedFunction = Func liftedLabel fArgs $ liftedFunctionBody
  return $ L4 closure (liftedFunction : moreFunctions)

goEs :: [L5.E] -> State Supply ([L4.E], [[L4Func]])
goEs es = unzip <$> fmap extract <$> traverse go es where
  extract (L4 e fs) = (e, fs)

toLabel :: Variable -> Label
toLabel (Variable v) = Label v

-- | Wraps an array with lets that grab their value from the array
-- | ex: let x = a !! 0 in let y = a !! 1 ... in e
wrapWithLets :: Variable -> [Variable] -> L4.E -> L4.E
wrapWithLets tup vars e = foldr f e (zip vars [0..]) where
  f (v, i) b = L4.Let v (L4.PrimApp ARef [VE $ VarV tup, VE $ NumV i]) b

{-
 In this case, we know we don't have a primitive function
 in the function position, so we must have a closure created
 from the Lambda case.

 (e0 e1 ... en) =>
 (let ([f e0]) ((closure-proc f) (closure-vars f) e1 ... en))
 -}

{-
Find all the free vars in an expression.
@param e  The expression
@return a list which contains all the free variables in e
-}
freeVars :: L5 -> [Variable]
freeVars e = Set.toList $ f e Set.empty where
  f (L5.Lambda args e)     bv = f e (Set.union (Set.fromList args) bv)
  f (Var v)                bv = if Set.member v bv then Set.empty else Set.singleton v
  f (L5.Let x r body)      bv = Set.union  (f r bv) (f body $ Set.insert x bv)
  f (L5.LetRec x r body)   bv = Set.union  (f r bv') (f body bv') where bv' = Set.insert x bv
  f (L5.If e a b) bv =     Set.unions [f e bv, f a bv, f b bv]
  f (L5.NewTuple es)       bv = Set.unions (flip f bv <$> es)
  f (L5.Begin e1 e2)       bv = Set.union  (f e1 bv) (f e2 bv)
  f (L5.App e es)          bv = Set.union (f e bv) (Set.unions (flip f bv <$> es))
  f (LitInt _)             _  = Set.empty
  f (PrimE _)              _  = Set.empty

{- Replace all occurrences of x for y in e. -}
subst :: Variable -> L5 -> L5 -> L5
subst x y = f where
  f (Lambda vs e)     = if x `elem` vs then Lambda vs e else Lambda vs (f e)
  f (Var v)           = if v == x then y else Var v
  f (L5.Let v e1 b)   = L5.Let v (f e1) (if v == x then b else f b)
  f (L5.LetRec v r b) = if v==x then L5.LetRec v r b else L5.LetRec v (f r) (f b)
  f (L5.If e a b)     = L5.If (f e) (f a) (f b)
  f (L5.NewTuple es)  = L5.NewTuple (f <$> es)
  f (L5.Begin e1 e2)  = L5.Begin (f e1) (f e2)
  f (L5.App e es)     = L5.App (f e) (f <$> es)
  f e@(LitInt _)      = e
  f e@(PrimE _)       = e


