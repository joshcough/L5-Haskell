module L.L1L2Parser 
  (
     l1Parser32
    ,l2Parser32
    ,l1Parser64
    ,l2Parser64
    ,parseL132
    ,parseL232
    ,parseL164
    ,parseL264
    ,parseL232InstList
    ,parseL264InstList
  ) where

import Control.Monad
import Data.Maybe
import Data.Traversable
import L.Read
import L.L1L2AST
import L.Utils

type ParseResult p = Either String p

data Mode = X32 | X64
data Parser x s = Parser {
  mode   :: Mode,
  parseX :: String -> ParseResult x,
  parseS :: String -> ParseResult s
}

parseProgram :: Parser x s -> SExpr -> ParseResult (Program x s)
parseProgram p (List ((List main) : funcs)) = do
  main'  <- (parseMain p) main
  funcs' <- traverse (parseFunction p) funcs
  return $ Program main' funcs'
parse _ bad = Left $ "bad program" ++ show bad

parseMain :: Parser x s -> [SExpr] -> ParseResult (Func x s)
parseMain p exps =
  do { body <- traverse (parseI p) exps; return $ Func $ (LabelDeclaration "main") : body }

parseFunction :: Parser x s -> SExpr -> ParseResult (Func x s)
parseFunction p (List ((AtomSym name) : exps)) =
  do { body <- traverse (parseI p) exps; return $ Func $ LabelDeclaration (parseLabel name) : body }

parseInstructionList :: Parser x s -> SExpr -> ParseResult [(Instruction x s)]
parseInstructionList p (List exps) = traverse (parseI p) exps
parseInstructionList p _ = Left "not an instruction list"

parseI :: Parser x s -> SExpr -> ParseResult (Instruction x s)
parseI p a@(AtomSym s) = maybe
  (Left $ "not an instruction " ++ show a)
  id
  (parseLabelOrRegister
    (mode p)
    (Right . LabelDeclaration)
    (const $ Left $ "expected a label " ++ show a)
    s
  )
parseI p a@(AtomNum n) = Left $ "bad instruction" ++ show a
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

parseX86Operator :: String -> ParseResult X86Op
parseX86Operator "+="  = Right increment
parseX86Operator "-="  = Right decrement
parseX86Operator "*="  = Right multiply
parseX86Operator "<<=" = Right leftShift
parseX86Operator ">>=" = Right rightShift
parseX86Operator "&="  = Right bitwiseAnd
parseX86Operator _     = Left "bad operator"

parseLabel :: String -> Label
parseLabel s = drop 1 s

parseLabelOrRegister :: Mode -> (Label -> a) -> (Register -> a) -> String -> Maybe a
parseLabelOrRegister _ f _ l@(':' : _) = Just $ f $ parseLabel l
parseLabelOrRegister m _ f r           = fmap f $ parseRegister m r

parseRegister :: Mode -> String -> Maybe Register
parseRegister X32 r = do
  reg <- registerFromName r
  if is32Bit reg then Just reg else Nothing
parseRegister X64 r = registerFromName r

-- todo add check for divisible by 4
parseN4 n = case (sread n) of
  AtomNum n -> Right n
  AtomSym s -> Left $ "not a number" ++ n
parseCXRegister cx = fmap CXR (cxRegisterFromName cx)

-- L1 Parser (uses shared L1/L2 Parser)
l1Parser32 = l1Parser X32
l1Parser64 = l1Parser X64
l1Parser mode = Parser mode parseL1Reg parseL1S where
  parseL1Reg s = maybe (Left $ "invalid register: " ++ s) Right (parseRegister mode s)
  parseL1S s = case (sread s) of
    AtomNum n -> Right $ NumberL1S n
    AtomSym s -> maybe (Left $ "invalid s: " ++ s) Right $ parseLabelOrRegister mode LabelL1S RegL1S s

parseL132 = parseProgram l1Parser32
parseL132InstList = parseInstructionList l1Parser32
parseL164 = parseProgram l1Parser64
parseL164InstList = parseInstructionList l1Parser64

-- L2 Parser (uses shared L1/L2 Parser)
l2Parser32 = l2Parser X32
l2Parser64 = l2Parser X64
l2Parser mode = Parser mode (parseX VarL2X RegL2X) parseL2S where
  parseX v r  s = Right $ maybe (v s) r (parseRegister mode s)
  parseL2S    s = case (sread s) of
    AtomNum n -> Right $ NumberL2S n
    AtomSym s -> maybe (parseX (XL2S . VarL2X) (XL2S . RegL2X) s) Right $
                   parseLabelOrRegister mode LabelL2S (XL2S . RegL2X) s

parseL232 = parseProgram l2Parser32
parseL232InstList = parseInstructionList l2Parser32
parseL264 = parseProgram l2Parser64
parseL264InstList = parseInstructionList l2Parser64
