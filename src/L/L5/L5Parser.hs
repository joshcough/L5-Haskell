module L.L5.L5Parser (parseL5) where

import Control.Applicative
import Control.Monad
import Data.Traversable hiding (sequence)
import L.L1L2AST (Variable, Label)
import L.Read
import L.L3.L3AST (PrimName(..), V(..), primName, primByRawNameMaybe)
import L.L5.L5AST

parseL5 :: SExpr -> ParseResult L5
parseL5 = parseE

parseArg :: SExpr -> ParseResult String
parseArg (AtomSym s) = Right s
parseArg bad = Left $ "bad L5-variable" ++ show bad

parseE :: SExpr -> ParseResult E
parseE (List [AtomSym "lambda", List args, b]) = liftM2 Lambda (traverse parseArg args)  (parseE b)
parseE (List [AtomSym "let",    List [List [arg, e]], b]) = liftM3 Let    (parseArg arg) (parseE e) (parseE b)
parseE (List [AtomSym "letrec", List [List [arg, e]], b]) = liftM3 LetRec (parseArg arg) (parseE e) (parseE b)
parseE (List [AtomSym "if", pe, te, fe])  = liftM3 IfStatement (parseE pe) (parseE te) (parseE fe)
parseE (List (AtomSym "new-tuple" : es))  = liftM  NewTuple (traverse parseE es)
parseE (List [AtomSym "begin", e1, e2])   = liftM2 Begin (parseE e1) (parseE e2)
parseE (List (e : es)) = liftM2 App (parseE e) (traverse parseE es)
parseE (AtomSym v) = Right $ maybe (Var v) (PrimE . primName) (primByRawNameMaybe v)
parseE (AtomNum n) = Right $ LitInt (fromIntegral n)
