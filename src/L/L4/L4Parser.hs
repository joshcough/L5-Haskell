module L.L4.L4Parser (parseL4) where

import Control.Applicative
import Control.Monad
import Data.Traversable hiding (sequence)
import L.L1L2AST (Variable, Label)
import L.Read
import L.L3.L3AST (PrimName(..), V(..), Func(..))
import L.L4.L4AST

l4ParseError :: String -> SExpr -> Either String a
l4ParseError msg exp = Left $ concat ["L4 Parse Error: '", msg, "' in: ", show exp]

-- p ::= (e (l (x ...) e) ...)
parseL4 :: SExpr -> ParseResult L4
parseL4 (List (main : funcs)) = liftM2 L4 (parseE main) (traverse parseFunction funcs)
parseL4 bad = l4ParseError "bad L4-program" bad

parseLabel :: SExpr -> ParseResult Label
parseLabel (AtomSym l@(':' : _)) = Right $ l
parseLabel bad = l4ParseError "bad L4-label" bad

-- (l (x ...) e)PrimName
parseFunction :: SExpr -> ParseResult L4Func
parseFunction (List [l, args, e]) = liftM3 Func (parseLabel l) (parseArgs args) (parseE e) where
  parseArgs :: SExpr -> ParseResult [Variable]
  parseArgs (List args)  = sequence (parseArg <$> args) where
  parseArgs bad = l4ParseError "bad L4 argument list" bad
parseFunction bad = l4ParseError "bad L4-function" bad

parseArg :: SExpr -> ParseResult String
parseArg (AtomSym s) = Right s
parseArg bad = l4ParseError "bad L4-variable" bad

-- v :: = x | l | num
parseV :: SExpr -> ParseResult V
parseV (AtomSym l@(':' : _)) = Right $ LabelV l
parseV (AtomSym v) = Right $ VarV v
parseV (AtomNum n) = Right $ NumV (fromIntegral n)
parseV bad         = l4ParseError "bad L4-V" bad

parseE :: SExpr -> ParseResult E
parseE (List [AtomSym "let", List [List [arg, e]], b]) = liftM3 Let (parseArg arg) (parseE e) (parseE b)
parseE (List [AtomSym "if", pe, te, fe])     = liftM3 IfStatement (parseE pe) (parseE te) (parseE fe)
parseE (List (AtomSym "new-tuple" : es))     = liftM  NewTuple (traverse parseE es)
parseE (List [AtomSym "begin", e1, e2])      = liftM2 Begin (parseE e1) (parseE e2)
parseE (List [AtomSym "make-closure", l, e]) = liftM2 MakeClosure (parseLabel l) (parseE e)
parseE (List [AtomSym "closure-proc", e])    = liftM  ClosureProc (parseE e)
parseE (List [AtomSym "closure-vars", e])    = liftM  ClosureVars (parseE e)
-- TODO: this is bad because if they give the wrong number of args to a prim,
-- TODO: it'll parse a FunCall
parseE (List [AtomSym "new-array", s, e])    = parsePrimApp2 NewArray s e
parseE (List [AtomSym "aref", a, loc])       = parsePrimApp2 ARef a loc
parseE (List [AtomSym "aset", a, loc, e])    = parsePrimApp3 ASet a loc e
parseE (List [AtomSym "alen", e])            = parsePrimApp1 ALen e
parseE (List [AtomSym "print", e])           = parsePrimApp1 Print e
parseE (List [AtomSym "+",    l, r])         = parsePrimApp2 Add l r
parseE (List [AtomSym "-",    l, r])         = parsePrimApp2 Sub l r
parseE (List [AtomSym "*",    l, r])         = parsePrimApp2 Mult l r
parseE (List [AtomSym "<",    l, r])         = parsePrimApp2 LessThan l r
parseE (List [AtomSym "<=",   l, r])         = parsePrimApp2 LTorEQ l r
parseE (List [AtomSym "=",    l, r])         = parsePrimApp2 EqualTo l r
parseE (List [AtomSym "number?", e])         = parsePrimApp1 IsNumber e
parseE (List [AtomSym "a?",      e])         = parsePrimApp1 IsArray e
parseE (List (e : es)) = liftM2 FunCall (parseE e) (traverse parseE es)
parseE v = VE <$> parseV v

parsePrimApp1 :: PrimName -> SExpr -> ParseResult E
parsePrimApp1 p e = liftM (PrimApp p . return) (parseE e)

parsePrimApp2 :: PrimName -> SExpr -> SExpr -> ParseResult E
parsePrimApp2 b l r = liftM2 (\v1 v2 -> PrimApp b [v1,v2]) (parseE l) (parseE r)

parsePrimApp3 :: PrimName -> SExpr -> SExpr -> SExpr -> ParseResult E
parsePrimApp3 b e1 e2 e3 =
  liftM3 (\v1 v2 v3 -> PrimApp b [v1,v2,v3]) (parseE e1) (parseE e2) (parseE e3)