module L.L1L2Parser 
  (
     l1Parser
    ,l2Parser
    ,parseL1
    ,parseL2
    ,parseL2InstList
  ) where

import Control.Monad
import Data.Maybe
import Data.Traversable
import L.Read
import L.L1L2AST
import L.Utils

data Parser x s = Parser {
  level  :: String,
  parseX :: String -> ParseResult x,
  parseS :: String -> ParseResult s
}

parseError :: String -> Parser x s -> String -> Either String a
parseError  msg p exp = Left $ concat [level p, " Parse Error: '", msg, "' in: ", show exp]
parseError_ :: String -> Parser x s -> SExpr -> Either String a
parseError_ msg p exp = parseError msg p (show exp)

parseProgram :: Parser x s -> SExpr -> ParseResult (Program x s)
parseProgram p (List ((List main) : funcs)) = do
  main'  <- (parseMain p) main
  funcs' <- traverse (parseFunction p) funcs
  return $ Program main' funcs'
parse p bad = parseError_ "bad program" p bad

parseMain :: Parser x s -> [SExpr] -> ParseResult (Func x s)
parseMain p exps =
  do { body <- traverse (parseI p) exps; return $ Func $ (LabelDeclaration "main") : body }

parseFunction :: Parser x s -> SExpr -> ParseResult (Func x s)
parseFunction p (List ((AtomSym name) : exps)) =
  do { body <- traverse (parseI p) exps; return $ Func $ LabelDeclaration (parseLabel name) : body }

parseInstructionList :: Parser x s -> SExpr -> ParseResult [(Instruction x s)]
parseInstructionList p (List exps) = traverse (parseI p) exps
parseInstructionList p bad = parseError_ "not an instruction list" p bad

parseI :: Parser x s -> SExpr -> ParseResult (Instruction x s)
parseI p a@(AtomSym s) = maybe
  (parseError_ "not an instruction " p a)
  id
  (parseLabelOrRegister
    (Right . LabelDeclaration)
    (const $ parseError_ "expected a label " p a)
    s
  )
parseI p a@(AtomNum n) = parseError_ "bad instruction" p a
parseI p l@(List ss) = case (flatten l) of
  [x, "<-", "print", s] -> do
    x' <- (parseX p) x
    s' <- (parseS p) s
    return $ Assign x' (Print s')
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
  [x, "<-", s]  -> do
    x' <- (parseX p) x
    s' <- (parseS p) s
    return $ Assign x' $ SRHS s'
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
  [x, op, s] -> do 
    x' <- (parseX p) x
    s' <- (parseS p) s
    op' <- parseX86Operator op
    return $ MathInst x' op' s'
  [cx, "<-", s1, cmp, s2] -> do
    cx'  <- (parseX p) cx
    cmp' <- parseComp s1 cmp s2
    return $ Assign cx' $ CompRHS cmp'
  ["goto", l] -> Right $ Goto (parseLabel l)
  ["cjump", s1, cmp, s2, l1, l2] -> do
    cmp' <- parseComp s1 cmp s2
    return $ CJump cmp' (parseLabel l1) (parseLabel l2)
  ["call", s]      -> do
    s' <- (parseS p) s
    return $ Call s'
  ["tail-call", s] -> do
    s' <- (parseS p) s
    return $ TailCall s'
  ["return"]       -> Right Return
  _ -> parseError_ "bad instruction" p l
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
    parseX86Operator :: String -> ParseResult X86Op
    parseX86Operator "+="  = Right increment
    parseX86Operator "-="  = Right decrement
    parseX86Operator "*="  = Right multiply
    parseX86Operator "<<=" = Right leftShift
    parseX86Operator ">>=" = Right rightShift
    parseX86Operator "&="  = Right bitwiseAnd
    parseX86Operator bad   = parseError_ "bad operator" p (AtomSym bad)
    -- todo add check for divisible by 4
    parseN4 = liftP f where
      f (AtomNum n) = Right n
      f bad = parseError_ "not a number" p bad

parseLabel :: String -> Label
parseLabel s = drop 1 s

parseLabelOrRegister :: (Label -> a) -> (Register -> a) -> String -> Maybe a
parseLabelOrRegister f _ l@(':' : _) = Just $ f $ parseLabel l
parseLabelOrRegister _ f r           = fmap f $ parseRegister r

parseRegister :: String -> Maybe Register
parseRegister = registerFromName

-- L1 Parser (uses shared L1/L2 Parser)
l1Parser :: Parser L1X L1S
l1Parser = Parser "L1" parseL1Reg (liftP parseL1S) where
  parseL1Reg s = maybe (parseError "invalid register" l1Parser s) Right (parseRegister s)
  parseL1S (AtomNum n) = Right $ NumberL1S (fromIntegral n)
  parseL1S (AtomSym s) = maybe (parseError "invalid s" l1Parser s) Right $ parseLabelOrRegister LabelL1S RegL1S s

parseL1 = parseProgram l1Parser
parseL1InstList = parseInstructionList l1Parser

-- L2 Parser (uses shared L1/L2 Parser)
l2Parser :: Parser L2X L2S
l2Parser = Parser "L2" (parseX VarL2X RegL2X) (liftP parseL2S) where
  parseX v r  s = Right $ maybe (v s) r (parseRegister s)
  parseL2S (AtomNum n) = Right $ NumberL2S (fromIntegral n)
  parseL2S (AtomSym s) = maybe (parseX (XL2S . VarL2X) (XL2S . RegL2X) s) Right $
                           parseLabelOrRegister LabelL2S (XL2S . RegL2X) s

parseL2 = parseProgram l2Parser
parseL2InstList = parseInstructionList l2Parser
