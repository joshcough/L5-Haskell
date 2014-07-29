module L.L4.L4Parser (parseL4) where

import Control.Applicative
import Control.Monad
import Data.Traversable hiding (sequence)
import L.L1L2AST (Variable, Label)
import L.Read
import L.L4.L4AST

l4ParseError :: String -> SExpr -> Either String a
l4ParseError msg exp = Left $ concat ["L4 Parse Error: '", msg, "' in: ", show exp]

-- p ::= (e (l (x ...) e) ...)
parseL4 :: SExpr -> ParseResult L4
parseL4 (List (main : funcs)) = liftM2 L4 (parseE main) (traverse parseFunction funcs)
parse bad = l4ParseError "bad L4-program" bad

parseLabel :: SExpr -> ParseResult Label
parseLabel (AtomSym (':' : name)) = Right $ name
parseLabel bad = l4ParseError "bad L4-label" bad

-- (l (x ...) e)
parseFunction :: SExpr -> ParseResult Func
parseFunction (List [l, args, e]) = liftM3 Func (parseLabel l) (parseArgs args) (parseE e) where
  parseArgs :: SExpr -> ParseResult [Variable]
  parseArgs (List args)  = sequence (parseArg <$> args) where
  parseArgs bad = l4ParseError "bad L4 argument list" bad
parseFunction bad = l4ParseError "bad L4-function" bad

parseArg (AtomSym s) = Right s
parseArg bad = l4ParseError "bad L4-variable" bad

-- v :: = x | l | num
parseV :: SExpr -> ParseResult V
parseV (AtomSym (':' : rest)) = Right $ LabelV rest
parseV (AtomSym v) = Right $ VarV v
parseV (AtomNum n) = Right $ NumV (fromIntegral n)
parseV bad         = l4ParseError "bad L4-V" bad

parseE :: SExpr -> ParseResult E
parseE (List [AtomSym "let", List [List [arg, e]], b]) = liftM3 Let (parseArg arg) (parseE e) (parseE b)
parseE (List [AtomSym "if", pe, te, fe])     = liftM3 IfStatement (parseE pe) (parseE te) (parseE fe)
parseE (List [AtomSym "new-array", s, e])    = liftM2 NewArray (parseE s) (parseE e)
parseE (List (AtomSym "new-tuple" : es))     = liftM  NewTuple (traverse parseE es)
parseE (List [AtomSym "aref", a, loc])       = liftM2 ARef  (parseE a) (parseE loc)
parseE (List [AtomSym "aset", a, loc, e])    = liftM3 ASet  (parseE a) (parseE loc) (parseE e)
parseE (List [AtomSym "alen", e])            = liftM  ALen  (parseE e)
parseE (List [AtomSym "print", e])           = liftM  Print (parseE e)
parseE (List [AtomSym "begin", e1, e2])      = liftM2 Begin (parseE e1) (parseE e2)
parseE (List [AtomSym "make-closure", l, e]) = liftM2 MakeClosure (parseLabel l) (parseE e)
parseE (List [AtomSym "closure-proc", e])    = liftM  ClosureProc (parseE e)
parseE (List [AtomSym "closure-vars", e])    = liftM  ClosureVars (parseE e)
parseE (List [AtomSym "+",    l, r])         = parseBiop Add l r
parseE (List [AtomSym "-",    l, r])         = parseBiop Sub l r
parseE (List [AtomSym "*",    l, r])         = parseBiop Mult l r
parseE (List [AtomSym "<",    l, r])         = parseBiop LessThan l r
parseE (List [AtomSym "<=",   l, r])         = parseBiop LTorEq l r
parseE (List [AtomSym "=",    l, r])         = parseBiop Eq l r
parseE (List [AtomSym "number?", e])         = parsePred IsNum e
parseE (List [AtomSym "a?",      e])         = parsePred IsArray e
parseE (List (e : es)) = liftM2 FunCall (parseE e) (traverse parseE es)
parseE v = VE <$> parseV v

parseBiop :: (E -> E -> Biop) -> SExpr -> SExpr -> ParseResult E
parseBiop c l r = BiopE <$> liftM2 c (parseE l) (parseE r)

parsePred :: (E -> Pred) -> SExpr -> ParseResult E
parsePred c e = PredE <$> liftM c (parseE e)
