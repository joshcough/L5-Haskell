module L.L1L2Parser 
  (
    Parser(..),
    parseI,
    parseProgram,
    parseRegister,
    parseLabelOrRegister
  ) where

import L.Read
import L.L1L2AST
import Data.Maybe
import Data.Traversable

data Parser x s = Parser {
  parseX :: String -> Either String x,
  parseS :: String -> Either String s
}

parseProgram :: Parser x s -> SExpr -> Either String (Program x s)
parseProgram p (List ((List main) : funcs)) = do
  main'  <- (parseMain p) main
  funcs' <- traverse (parseFunction p) funcs
  return $ Program main' funcs'
parse _ bad = Left $ "bad program" ++ show bad

parseMain :: Parser x s -> [SExpr] -> Either String (Func x s)
parseMain p exps =
  do { body <- traverse (parseI p) exps; return $ Func $ (LabelDeclaration "main") : body }

parseFunction :: Parser x s -> SExpr -> Either String (Func x s)
parseFunction p (List ((AtomSym name) : exps)) =
  do { body <- traverse (parseI p) exps; return $ Func $ LabelDeclaration (parseLabel name) : body }

parseI :: Parser x s -> SExpr -> Either String (Instruction x s)
parseI p a@(AtomSym s) = maybe
  (Left $ "not an instruction " ++ show a)
  id
  (parseLabelOrRegister
    (Right . LabelDeclaration)
    (const $ Left $ "expected a label " ++ show a)
    s
  )
parseI p a@(AtomNum n) = Left $ "bad instruction" ++ show a
parseI p l@(List ss) = case (flatten l) of
  [x, "<-", "print", s] -> do { x' <- (parseX p) x; s' <- (parseS p) s; return $ Assign x' (Print s') }
  [x, "<-", "allocate", s1, s2] -> do
    x'  <- (parseX p) x
    s1' <- (parseS p) s1
    s2' <- (parseS p) s2
    return $ Assign x' $ Allocate s1' s2'
  [x, "<-", "array-error", s1, s2] -> do
    x'  <- (parseX p) x
    s1' <- (parseS p) s1
    s2' <- (parseS p) s2
    return $ Assign x' $ ArrayError s1' s2'
  [x, "<-", s]  -> do { x' <- (parseX p) x; s' <- (parseS p) s; return $ Assign x' $ SRHS s' }
  [x1, "<-", "mem", x2, n4] -> do
    x1' <- (parseX p) x1
    x2' <- (parseX p) x2
    n4' <- parseN4 n4
    return $ Assign x1' $ MemRead $ MemLoc x2' n4'
  ["mem", x, n4, "<-", s] -> do
    x'  <- (parseX p) x
    n4' <- parseN4 n4
    s'  <- (parseS p) s
    return $ MemWrite (MemLoc x' n4') s'
  [x, op, s] -> do { x' <- (parseX p) x; s' <- (parseS p) s; op' <- parseX86Operator op; return $ MathInst x' op' s' }
  [cx, "<-", s1, cmp, s2] -> do
    cx'  <- (parseX p) cx
    cmp' <- parseComp s1 cmp s2
    return $ Assign cx' $ CompRHS cmp'
  ["goto", l] -> Right $ Goto (parseLabel l)
  ["cjump", s1, cmp, s2, l1, l2] -> do
    cmp' <- parseComp s1 cmp s2
    return $ CJump cmp' (parseLabel l1) (parseLabel l2)
  ["call", s]      -> do { s' <- (parseS p) s; return $ Call s' }
  ["tail-call", s] -> do { s' <- (parseS p) s; return $ TailCall s' }
  ["return"]       -> Right Return
  xs -> Left $ "bad instruction" ++ show l
  {-
  I wanted these at the bottom, but had to move them to the top
  because of some weird compiler error.
  parseB :: [String] -> String
  parseB ["eax", "<-", "allocate", t1, t2] = "allocate"
  parseB ["eax", "<-", "array-error", t1, t2] = "array-error"
  -}
  where
    parseComp s1 cmp s2 = do
      s1'  <- (parseS p) s1
      cmp' <- compOpFromSym cmp
      s2'  <- (parseS p) s2
      return $ Comp s1' cmp' s2'

parseX86Operator "+="  = Right increment
parseX86Operator "-="  = Right decrement
parseX86Operator "*="  = Right multiply
parseX86Operator "<<=" = Right leftShift
parseX86Operator ">>=" = Right rightShift
parseX86Operator "&="  = Right bitwiseAnd
parseX86Operator _     = Left "bad operator"

parseLabel :: String -> Label
parseLabel s = drop 1 s

parseLabelOrRegister :: (Label -> a) -> (Register -> a) -> String -> Maybe a
parseLabelOrRegister f _ l@(':' : _) = Just $ f $ parseLabel l
parseLabelOrRegister _ f r           = fmap f $ parseRegister r

parseRegister :: String -> Maybe Register
parseRegister r = registerFromName r
parseN4 n = case (sread n) of
  AtomNum n -> Right n
  AtomSym s -> Left $ "not a number" ++ n
parseCXRegister cx = fmap CXR (cxRegisterFromName cx)
