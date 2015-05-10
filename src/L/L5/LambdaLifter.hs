{-# LANGUAGE TypeFamilies #-}

module L.L5.LambdaLifter (lambdaLift) where

import Bound
import Bound.Var
import Control.Applicative
import Control.Lens
import Control.Monad.State
import Data.Monoid
import qualified Data.Set as Set
import L.Primitives
import L.L1.L1L2AST (Label(..))
import L.L4.L4AST as L4
import L.L5.L5AST as L5
import L.Parser.SExpr
import L.Parser.Supply
import L.Variable hiding (freeVars)

lambdaLift :: L5 -> L4
lambdaLift l5 = fst $ runState (go id l5) (newSupply $ boundVars l5 <> freeVars l5)

{-
TODO: wat? how did f and x get reversed? er, just...waaat?
*L.ReplTools> quickCompileL5 "(f x)"
"((let (x f) ((closure-proc x) (closure-vars x) x)))"
-}

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

primToLambda :: PrimName -> L5.E (Var Int a)
primToLambda p = l where
  a = App (PrimE p) [Var (B 0), Var (B 1)]
  l = Lambda [Variable "x", Variable "y"] (abstract (^? _B) a)

l5c :: String -> L4
l5c s = fst $ runState (go id l5e) (newSupply $ boundVars l5e <> freeVars l5e) where
  l5e = (either error id) . fromSExpr $ sread s

go :: Ord a => (a -> Variable) -> L5.E a -> State Supply L4
go _ (LitInt i)         = return $ L4 (VE . NumV $ fromIntegral i) []
go f (Var v)            = return $ L4 (VE . VarV $ f v) []
go f (L5.If e t g)      = do
  (L4 e' efs) <- go f e
  (L4 t' tfs) <- go f t
  (L4 g' gfs) <- go f g
  return $ L4 (L4.IfStatement e' t' g') (efs++tfs++gfs)
go f (L5.Begin e1 e2)   = do
  (L4 e1' e1fs) <- go f e1
  (L4 e2' e2fs) <- go f e2
  return $ L4 (L4.Begin e1' e2') (e1fs++e2fs)
go f (L5.NewTuple es)   = do
  (l4es, fs) <- goEs f es
  return $ L4 (L4.NewTuple l4es) (concat fs)
go f (L5.Let v e b)     = do
  v'          <- freshNameForS v
  (L4 e' efs) <- go f e
  (L4 b' bfs) <- go (unvar (const v') f) (fromScope b)
  return $ L4 (L4.Let v' e' b') (efs++bfs)
{-
 We Turn (f +) => (f (lambda (x y) (+ x y)))
 So when we see a primitive function by itself,
 turn it into a lambda expression, and then compile that expression.
 -}
go f (PrimE p) = go (unvar (primVars p !!) f) (primToLambda p)
{-
 Primitive function application
 (+ x y z)  => (+ x y z)
 also a trivial case, but I wanted to keep this case close to the other App case.
-}
go f (App (PrimE p) es) = do
  (es', fs) <- goEs f es
  return $ L4 (L4.PrimApp p es') (concat fs)
go f (App e es) = do
  v' <- freshNameForS' "x"
  -- compile the function position
  (L4 e' fs') <- go f e
  -- compile all of the arguments
  (es', fs'') <- goEs f es
  let ve = VE $ VarV v'
  return $ L4 (L4.Let v' e' (
    -- free variables go in the first argument.
    L4.FunCall (L4.ClosureProc ve) $
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
  
 Lambda [Variable] (Scope Int E a)
-}
go f lam@(Lambda args body) = do
  {- build the body of the new function -}
  -- compile the body of the lambda
  (L4 compiledLambdaBody moreFunctions) <- go (unvar (args !!) f) (fromScope body)
  let usingArgsTuple = length args > 2
      freesVar = Variable "frees"
      argsVar  = Variable "args"
      -- the arguments to the new function
      fArgs = if usingArgsTuple then [freesVar, argsVar] else freesVar : args
      -- the free variables in the lambda
      frees = Set.toList $ freeVars lam
      -- then wrap it with the required let statements
      freeLets = wrapWithLets freesVar (error "todo:158" frees) compiledLambdaBody
      liftedFunctionBody = if   usingArgsTuple 
                           then wrapWithLets argsVar args freeLets 
                           else freeLets
  liftedLabel <- fmap toLabel (supplyNameFor' "x")
  let closure = L4.MakeClosure liftedLabel $ L4.NewTuple (VE . VarV <$> (error "todo:163" frees))
      liftedFunction = Func liftedLabel fArgs $ liftedFunctionBody
  return $ L4 closure (liftedFunction : moreFunctions)

goEs :: Ord a => (a -> Variable) -> [L5.E a] -> State Supply ([L4.E], [[L4Func]])
goEs f es = unzip <$> fmap extract <$> traverse (go f) es where 
  extract (L4 e fs) = (e, fs)

toLabel :: Variable -> Label
toLabel (Variable v) = Label v

-- | Wraps an array with lets that grab their value from the array
-- | ex: let x = a !! 0 in let y = a !! 1 ... in e
wrapWithLets :: Variable -> [Variable] -> L4.E -> L4.E
wrapWithLets tup vars e = foldr f e (zip vars [0..]) where
  f (v, i) b = L4.Let v (L4.PrimApp ARef [VE $ VarV tup, VE $ NumV i]) b

{-
 letrec doesn't exist in L4, so we do this simple transformation.
 (letrec ([x e1]) e2)
  =>
 (let ([x (new-tuple 0)])
   (begin (aset x 0 e1[x:=(aref x 0)])
          e2[x:=(aref x 0)]))
compile (LetRec v e b) = compile $
  L5.Let v (L5.NewTuple [LitInt 0]) (L5.Begin
    (App (PrimE ASet) [Var v, LitInt 0, subst v (App (PrimE ARef) [Var v, LitInt 0]) e])
    (subst v (App (PrimE ARef) [Var v, LitInt 0]) b)
  )
-}

{-
 In this case, we know we don't have a primitive function
 in the function position, so we must have a closure created
 from the Lambda case.

 (e0 e1 ... en) =>
 (let ([f e0]) ((closure-proc f) (closure-vars f) e1 ... en))
 -}
{-
compileOld (App f args) = do
  v <- newVar
  -- compile the function position
  (L4 compiledF fs')   <- compileOld f
  -- compile all of the arguments
  (compiledArgs, fs'') <- compileEs args
  let ve = VE $ VarV v
  return $ L4 (L4.Let v compiledF (
    -- free variables go in the first argument.
    L4.FunCall (L4.ClosureProc $ ve) $
      -- if we can fit the rest of the arguments in, then great
      -- if not, they must also go into another tuple.
      L4.ClosureVars ve :
        (if length compiledArgs <= 2 then compiledArgs else [L4.NewTuple compiledArgs])
   )) (fs' ++ fs'')
-}

{- Replace all occurrences of x for y in e. -}
subst :: Variable -> L5 -> L5 -> L5
subst _ _ = error "todo"
{-
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
-}


