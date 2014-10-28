module L.L5.L5Parser (parseL5) where

import Control.Applicative
import Control.Monad
import Data.Traversable hiding (sequence)
import L.L1L2AST (Variable, Label)
import L.Read
import L.L3.L3AST (PrimName(..), V(..))
import L.L5.L5AST

l5ParseError :: String -> SExpr -> Either String a
l5ParseError msg exp = Left $ concat ["L5 Parse Error: '", msg, "' in: ", show exp]

parseL5 :: SExpr -> ParseResult L5
parseL5 = parseE

parseLabel :: SExpr -> ParseResult Label
parseLabel (AtomSym l@(':' : _)) = Right $ l
parseLabel bad = l5ParseError "bad L5-label" bad

parseArg :: SExpr -> ParseResult String
parseArg (AtomSym s) = Right s
parseArg bad = l5ParseError "bad L5-variable" bad

{-

e ::= (lambda (x ...) e)
    | x
    | (let ([x e]) e)
    | (letrec ([x e]) e)
    | (if e e e)
    | (new-tuple e ...)
    | (begin e e)
    | (e e ...) ;; application expression
    | prim
    | num

prim ::= biop
       | pred
       | print
       | new-array
       | aref
       | aset
       | alen

biop ::= + | - | * | < | <= | =
pred ::= number? | a?

data E =
    Lambda [Variable] E
  | Var Variable
  | Let Variable E E
  | LetRec Variable E E
  | IfStatement E E E
  | NewTuple [E]
  | Begin E E
  | App E [E]
  | LitInt Int
  | PrimE Prim
  deriving Eq

-}

parseE :: SExpr -> ParseResult E
parseE (List [AtomSym "lambda", List [List args], b]) = liftM2 Lambda (traverse parseArg args) (parseE b)
parseE (List [AtomSym "let",    List [List [arg, e]], b]) = liftM3 Let    (parseArg arg) (parseE e) (parseE b)
parseE (List [AtomSym "letrec", List [List [arg, e]], b]) = liftM3 LetRec (parseArg arg) (parseE e) (parseE b)
parseE (List [AtomSym "if", pe, te, fe])     = liftM3 IfStatement (parseE pe) (parseE te) (parseE fe)
parseE (List (AtomSym "new-tuple" : es))     = liftM  NewTuple (traverse parseE es)
parseE (List [AtomSym "begin", e1, e2])      = liftM2 Begin (parseE e1) (parseE e2)
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
-- TODO: this is bad because if they give the wrong number of args to a prim,
-- TODO: it'll parse a FunCall
parseE (List (e : es)) = liftM2 App (parseE e) (traverse parseE es)

parseE (AtomSym v) = Right $ Var v
parseE (AtomNum n) = Right $ LitInt (fromIntegral n)

parsePrimApp1 :: PrimName -> SExpr -> ParseResult E
parsePrimApp1 p e = liftM (App (PrimE p) . return) (parseE e)

parsePrimApp2 :: PrimName -> SExpr -> SExpr -> ParseResult E
parsePrimApp2 p l r = liftM2 (\v1 v2 -> App (PrimE p) [v1,v2]) (parseE l) (parseE r)

parsePrimApp3 :: PrimName -> SExpr -> SExpr -> SExpr -> ParseResult E
parsePrimApp3 p e1 e2 e3 = liftM3 (\v1 v2 v3 -> App (PrimE p) [v1,v2,v3]) (parseE e1) (parseE e2) (parseE e3)