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
parseD (List [AtomSym "make-closure", l, v]) = liftM2 MakeClosure (parseLabel l) (parseV v)
parseD (List [AtomSym "closure-proc", v])    = liftM  ClosureProc (parseV v)
parseD (List [AtomSym "closure-vars", v])    = liftM  ClosureVars (parseV v)
-- TODO: this is bad because if they give the wrong number of args to a prim,
-- TODO: it'll parse a FunCall
parseD (List [AtomSym "+",    l, r])         = parsePrimApp2 Add      l r
parseD (List [AtomSym "-",    l, r])         = parsePrimApp2 Sub      l r
parseD (List [AtomSym "*",    l, r])         = parsePrimApp2 Mult     l r
parseD (List [AtomSym "<",    l, r])         = parsePrimApp2 LessThan l r
parseD (List [AtomSym "<=",   l, r])         = parsePrimApp2 LTorEQ   l r
parseD (List [AtomSym "=",    l, r])         = parsePrimApp2 EqualTo  l r
parseD (List [AtomSym "number?", v])         = parsePrimApp1 IsNumber v
parseD (List [AtomSym "a?",      v])         = parsePrimApp1 IsArray  v
parseD (List [AtomSym "new-array", s, v])    = parsePrimApp2 NewArray s v
parseD (List (AtomSym "new-tuple" : vs))     = liftM  NewTuple (traverse parseV vs)
parseD (List [AtomSym "aref", a, loc])       = parsePrimApp2 ARef a loc
parseD (List [AtomSym "aset", a, loc, v])    = parsePrimApp3 ASet a loc v
parseD (List [AtomSym "alen", v])            = parsePrimApp1 ALen  v
parseD (List [AtomSym "print", v])           = parsePrimApp1 Print v
parseD (List (v : vs))                       = liftM2 FunCall (parseV v) (traverse parseV vs)
parseD v = VD <$> parseV v

parsePrimApp1 :: PrimName -> SExpr -> ParseResult D
parsePrimApp1 p v = liftM (PrimApp p . return) (parseV v)

parsePrimApp2 :: PrimName -> SExpr -> SExpr -> ParseResult D
parsePrimApp2 b l r = liftM2 (\v1 v2 -> PrimApp b [v1,v2]) (parseV l) (parseV r)

parsePrimApp3 :: PrimName -> SExpr -> SExpr -> SExpr -> ParseResult D
parsePrimApp3 b e1 e2 e3 =
  liftM3 (\v1 v2 v3 -> PrimApp b [v1,v2,v3]) (parseV e1) (parseV e2) (parseV e3)
