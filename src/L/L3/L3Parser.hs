module L.L3.L3Parser (parseL3) where

import Control.Applicative
import Control.Monad
import Data.Traversable hiding (sequence)
import L.L1L2AST (Variable, Label)
import L.Read
import L.L3.L3AST

l3ParseError :: String -> SExpr -> Either String a
l3ParseError msg exp = Left $ concat ["L3 Parse Error: '", msg, "' in: ", show exp]

-- p ::= (e (l (x ...) e) ...)
parseL3 :: SExpr -> ParseResult L3
parseL3 (List (main : funcs)) = liftM2 L3 (parseE main) (traverse parseFunction funcs)
parseL3 bad = l3ParseError "bad L3-program" bad

parseLabel :: SExpr -> ParseResult Label
parseLabel (AtomSym l@(':' : _)) = Right $ l
parseLabel bad = l3ParseError "bad L3-label" bad

-- (l (x ...) e)
parseFunction :: SExpr -> ParseResult Func
parseFunction (List [l, args, e]) = liftM3 Func (parseLabel l) (parseArgs args) (parseE e) where
  parseArgs :: SExpr -> ParseResult [Variable]
  parseArgs (List args)  = sequence (parseArg <$> args) where
  parseArgs bad = l3ParseError "bad L3 argument list" bad
parseFunction bad = l3ParseError "bad L3-function" bad

parseArg :: SExpr -> Either String String
parseArg (AtomSym s) = Right s
parseArg bad = l3ParseError "bad L3-variable" bad

-- v :: = x | l | num
parseV :: SExpr -> ParseResult V
parseV (AtomSym l@(':' : _)) = Right $ LabelV l
parseV (AtomSym v) = Right $ VarV v
parseV (AtomNum n) = Right $ NumV (fromIntegral n)
parseV bad         = l3ParseError "bad L3-V" bad

-- e ::= (let ([x d]) e) | (if v e e) | d
parseE :: SExpr -> ParseResult E
parseE (List [AtomSym "let", List [List [arg, d]], e]) = liftM3 Let (parseArg arg) (parseD d) (parseE e)
parseE (List [AtomSym "if", v, te, fe]) = liftM3 IfStatement (parseV v) (parseE te) (parseE fe)
parseE d = DE <$> parseD d

{-
d ::= (biop v v) | (pred v) | (v v ...) | (new-array v v) | (new-tuple v ...)
      (aref v v) | (aset v v v) | (alen v)
      (print v) | (make-closure l v) | (closure-proc v) | (closure-vars v)
      v
biop ::= + | - | * | < | <= | =
pred ::= number? | a?
 -}
parseD :: SExpr -> ParseResult D
parseD (List [AtomSym "+",    l, r]) = parseBiop Add l r
parseD (List [AtomSym "-",    l, r]) = parseBiop Sub l r
parseD (List [AtomSym "*",    l, r]) = parseBiop Mult l r
parseD (List [AtomSym "<",    l, r]) = parseBiop LessThan l r
parseD (List [AtomSym "<=",   l, r]) = parseBiop LTorEq l r
parseD (List [AtomSym "=",    l, r]) = parseBiop Eq l r
parseD (List [AtomSym "number?", v]) = parsePred IsNum v
parseD (List [AtomSym "a?",      v]) = parsePred IsArray v
parseD (List [AtomSym "new-array", s, v])    = liftM2 NewArray (parseV s) (parseV v)
parseD (List (AtomSym "new-tuple" : vs))     = liftM  NewTuple (traverse parseV vs)
parseD (List [AtomSym "aref", a, loc])       = liftM2 ARef  (parseV a) (parseV loc)
parseD (List [AtomSym "aset", a, loc, v])    = liftM3 ASet  (parseV a) (parseV loc) (parseV v)
parseD (List [AtomSym "alen", v])            = liftM  ALen  (parseV v)
parseD (List [AtomSym "print", v])           = liftM  Print (parseV v)
parseD (List [AtomSym "make-closure", l, v]) = liftM2 MakeClosure (parseLabel l) (parseV v)
parseD (List [AtomSym "closure-proc", v])    = liftM  ClosureProc (parseV v)
parseD (List [AtomSym "closure-vars", v])    = liftM  ClosureVars (parseV v)
parseD (List (v : vs)) = liftM2 FunCall (parseV v) (traverse parseV vs)
parseD v = VD <$> parseV v

parseBiop :: Biop -> SExpr -> SExpr -> ParseResult D
parseBiop b l r = liftM2 (BiopD b) (parseV l) (parseV r)

parsePred :: Pred -> SExpr -> ParseResult D
parsePred p v = liftM (PredD p) (parseV v)
