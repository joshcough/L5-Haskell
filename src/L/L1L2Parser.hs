module L.L1L2Parser 
  (
     l1Parser
    ,l2Parser
    ,parseL1
    ,parseL1OrDie
    ,parseL2
    ,parseL2OrDie
    ,parseL2InstList
  ) where

import Control.Applicative
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
parseProgram p (List ((List main) : funcs)) =
  liftM2 Program (parseMain p main) (traverse (parseFunction p) funcs)
parse p bad = parseError_ "bad program" p bad

parseMain :: Parser x s -> [SExpr] -> ParseResult (Func x s)
parseMain p exps = do
  body <- traverse (parseI p) exps
  return $ Func $ (LabelDeclaration "main") : body

parseFunction :: Parser x s -> SExpr -> ParseResult (Func x s)
parseFunction p (List ((AtomSym name) : exps)) = do
  body <- traverse (parseI p) exps
  return $ Func $ LabelDeclaration (parseLabel name) : body

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
  [x, "<-", "print", s] -> liftM2 Assign (parseX p x) (Print <$> parseS p s)
  [x, "<-", "allocate", s1, s2] ->
    liftM2 Assign (parseX p x) (liftM2 Allocate (parseS p s1) (parseS p s2))
  [x, "<-", "array-error", s1, s2] ->
    liftM2 Assign (parseX p x) (liftM2 ArrayError (parseS p s1) (parseS p s2))
  [x, "<-", s]  -> liftM2 Assign (parseX p x) (SRHS <$> parseS p s)
  [x1, "<-", "mem", x2, n4] ->
    liftM2 Assign (parseX p x1) (MemRead <$> liftM2 MemLoc (parseX p x2) (parseN4 n4))
  ["mem", x, n4, "<-", s] ->
    liftM2 MemWrite (liftM2 MemLoc (parseX p x) (parseN4 n4)) (parseS p s)
  [x, op, s] ->
    liftM3 MathInst (parseX p x) (parseX86Operator op) (parseS p s)
  [cx, "<-", s1, cmp, s2] -> 
    liftM2 Assign (parseX p cx) (CompRHS <$> parseComp s1 cmp s2)
  ["goto", l] -> Right $ Goto (parseLabel l)
  ["cjump", s1, cmp, s2, l1, l2] ->
    liftM3 CJump (parseComp s1 cmp s2) (parseLabelM l1) (parseLabelM l2)
  ["call", s] -> Call <$> parseS p s
  ["tail-call", s] -> TailCall <$> parseS p s
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
    parseComp s1 cmp s2 =
      liftM3 Comp (parseS p s1) (compOpFromSym cmp) (parseS p s2)
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

parseLabelM = return . parseLabel

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
parseL1OrDie = (either error id) . parseL1
parseL1InstList = parseInstructionList l1Parser

-- L2 Parser (uses shared L1/L2 Parser)
l2Parser :: Parser L2X L2S
l2Parser = Parser "L2" (parseX VarL2X RegL2X) (liftP parseL2S) where
  parseX v r  s = Right $ maybe (v s) r (parseRegister s)
  parseL2S (AtomNum n) = Right $ NumberL2S (fromIntegral n)
  parseL2S (AtomSym s) = maybe (parseX (XL2S . VarL2X) (XL2S . RegL2X) s) Right $
                           parseLabelOrRegister LabelL2S (XL2S . RegL2X) s

parseL2 = parseProgram l2Parser
parseL2OrDie = (either error id) . parseL2
parseL2InstList = parseInstructionList l2Parser

