module L.L5.LambdaLifter (lambdaLift) where

import Control.Applicative
import Control.Monad.State
import qualified Data.Set as Set
import Data.Traversable
import L.L1L2AST (Variable, Label)
import L.L3.L3AST as L3
import L.L4.L4AST as L4
import L.L5.L5AST as L5


lambdaLift :: L5 -> L4
lambdaLift l5 = fst $ runState (compile l5) 0

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
compile :: L5 -> State Int L4
compile (LitInt i)             = return $ L4 (VE . NumV $ fromIntegral i) []
compile (Var v)                = return $ L4 (VE $ VarV v) []
compile (L5.IfStatement e t f) = compileSubExprs [e,t,f] $ \es  -> L4.IfStatement (es !! 0) (es !! 1) (es !! 2)
compile (L5.Begin e1 e2)       = compileSubExprs [e1,e2] $ \es  -> L4.Begin (es !! 0) (es !! 1)
compile (L5.NewTuple es)       = compileSubExprs es      $ \es' -> L4.NewTuple es'
compile (L5.Let v e b)         = compileSubExprs [e,b]   $ \es  -> L4.Let v (es !! 0) (es !! 1)
{-
 letrec doesn't exist in L4, so we do this simple transformation.
 (letrec ([x e1]) e2)
  =>
 (let ([x (new-tuple 0)])
   (begin (aset x 0 e1[x:=(aref x 0)])
          e2[x:=(aref x 0)]))
-}
compile (LetRec v e b) = compile $
  L5.Let v (L5.NewTuple [LitInt 0]) (L5.Begin
    (App (PrimE ASet) [Var v, LitInt 0, subst v (App (PrimE ARef) [Var v, LitInt 0]) e])
    (subst v (App (PrimE ARef) [Var v, LitInt 0]) b)
  )
{-
 We Turn (f +) => (f (lambda (x y) (+ x y)))
 So when we see a primitive function by itself,
 turn it into a lambda expression, and then compile that expression.
 -}
compile pe@(PrimE p) = compile $ Lambda (primVars p) (App pe (Var <$> primVars p))
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
compile lam@(Lambda args body) = do
  let usingArgsTuple = length args > 2
      freesVar = "frees"
      argsVar  = "args"
      -- the arguments to the new function
      fArgs = if usingArgsTuple then [freesVar, argsVar] else freesVar : args
      -- the free variables in the lambda
      frees = freeVars lam
  {- build the body of the new function -}
  -- compile the body of the lambda
  (L4 compiledLambdaBody moreFunctions) <- compile body
  -- then wrap it with the required let statements
  let wrapWithLets tup vars e = foldr f e (zip vars [0..]) where
        f (v, i) b = L4.Let v (L4.PrimApp ARef [VE $ VarV tup, VE $ NumV i]) b
      freeLets = wrapWithLets freesVar frees compiledLambdaBody
      liftedFunctionBody = if usingArgsTuple then wrapWithLets argsVar args freeLets else freeLets where
  label <- newLabel
  let closure = L4.MakeClosure label $ L4.NewTuple (VE . VarV <$> frees)
      liftedFunction = L4.Func label fArgs $ liftedFunctionBody
  return $ L4 closure (liftedFunction : moreFunctions)

{-
 Primitive function application
 (+ x y z)  => (+ x y z)
 also a trivial case, but I wanted to keep this case close to the other App case.
 -}
compile (App (PrimE p) args) = compileSubExprs args $ \es  -> L4.PrimApp p es

{-
 In this case, we know we don't have a primitive function
 in the function position, so we must have a closure created
 from the Lambda case.

 (e0 e1 ... en) =>
 (let ([f e0]) ((closure-proc f) (closure-vars f) e1 ... en))
 -}
compile (App f args) = do
  v <- newVar
  -- compile the function position
  (L4 compiledF fs')   <- compile f
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

{-
This function is used to compile the subexpressions of and L5 e.
Once they are compiled, we need to construct the equivalent L4 e.
using the freshly compiled subexpressions.

For example, an L5 if-statement has three subexpressions
They get compiled into three L4 expressions (and some lifted functions)
which in turn, get shoved into an L4 if-statement.

@param es  The subexpressions to compile
@param createL4E  A function that takes the compiled subexpressions and
 creates the L4 expression as explained above
@return  a tuple containing one L4 expression and a list of L4 functions
        the functions are the lambdas lifted when compiling the es.
-}
compileSubExprs :: [L5.E] -> ([L4.E] -> L4.E) -> State Int L4
compileSubExprs es combine = do
  (l4es, fs) <- compileEs es
  return $ L4 (combine l4es) fs where

{-
Compile each of the given L5 expressions

@param es  A list of L5 es to be compiled
@return  a tuple containing
  a list of L4 es, one e for each of the incoming L5 es
  a list of L4 functions. the functions are the lambdas lifted when compiling the es.
-}
compileEs :: [L5.E] -> State Int ([L4.E], [L4.Func])
compileEs es = do
  let extract (L4 e fs) = (e, fs)
  (l4es, fs) <- unzip <$> fmap extract <$> traverse compile es
  return $ (l4es, concat fs)

{-
Find all the free vars in an expression.
@param e  The expression
@return a list which contains all the free variables in e
-}
freeVars :: L5.E -> [Variable]
freeVars e = Set.toList $ f e Set.empty where
  f (L5.Lambda args e)     bv = f e (Set.union (Set.fromList args) bv)
  f (Var v)                bv = if Set.member v bv then Set.empty else Set.singleton v
  f (L5.Let x r body)      bv = Set.union  (f r bv) (f body $ Set.insert x bv)
  f (L5.LetRec x r body)   bv = Set.union  (f r bv') (f body bv') where bv' = Set.insert x bv
  f (L5.IfStatement e a b) bv = Set.unions [f e bv, f a bv, f b bv]
  f (L5.NewTuple es)       bv = Set.unions (flip f bv <$> es)
  f (L5.Begin e1 e2)       bv = Set.union  (f e1 bv) (f e2 bv)
  f (L5.App e es)          bv = Set.union (f e bv) (Set.unions (flip f bv <$> es))
  f (LitInt _)             _  = Set.empty
  f (PrimE _)              _  = Set.empty

{- Replace all occurrences of x for y in e. -}
subst :: Variable -> L5.E -> L5.E -> L5.E
subst x y = f where
  f (Lambda vs e)          = if x `elem` vs then Lambda vs e else Lambda vs (f e)
  f (Var v)                = if v == x then y else Var v
  f (L5.Let v e1 b)        = L5.Let v (f e1) (if v == x then b else f b)
  f (L5.LetRec v r body)   = if v==x then L5.LetRec v r body else L5.LetRec v (f r) (f body)
  f (L5.IfStatement e a b) = L5.IfStatement (f e) (f a) (f b)
  f (L5.NewTuple es)       = L5.NewTuple (f <$> es)
  f (L5.Begin e1 e2)       = L5.Begin (f e1) (f e2)
  f (L5.App e es)          = L5.App (f e) (f <$> es)
  f e@(LitInt _)           = e
  f e@(PrimE _)            = e

newVar :: State Int Variable
newVar = incState "_l5_"
newLabel :: State Int Label
newLabel = incState ":l5_"
incState :: String -> State Int String
incState prefix = do { n <- get; put (n + 1); return $ prefix ++ show n }
