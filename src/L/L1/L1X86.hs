module L.L1.L1X86 (genX86Code, X86(..)) where

import Control.Applicative
import Control.Monad.Error
import Control.Monad.State
import Control.Monad.Trans.Except
import Data.Traversable
import L.L1.L1L2AST
import L.OS
import L.Primitives hiding (body, Print)
import L.Registers
import L.Util.Utils

-- X86 Generation code
type X86Inst     = String
type ProgramName = String

genX86Code :: ProgramName -> OS -> L1 -> Either String X86
genX86Code name os l1 = X86 <$> (fst $ runState (runExceptT $ genCodeS l1) 0) where
  genCodeS :: L1 -> ExceptT String (State Int) String
  genCodeS (L1 (Program main funcs)) = do
    x86Funcs <- compiledFunctions
    return $ dump $ concat [mainHeader, concat x86Funcs, ["\n"]] where

    compiledFunctions :: ExceptT String (State Int) [[X86Inst]]
    compiledFunctions = Data.Traversable.sequence $ fmap compile (main : funcs)

    dump :: [X86Inst] -> String
    dump insts = mkString "\n" $ map indent insts
    indent i = if (last i == ':' || take 6 i == ".globl") then i else '\t' : i
    mainHeader = [
      ".file\t\""++ name ++"\"",
      ".text",
      ".globl _go",
      "_go:" ]

  compile :: L1Func -> ExceptT String (State Int) [X86Inst]
  compile f = traverse genInstS (body f) >>= return . concat where

  genInstS :: L1Instruction -> ExceptT String (State Int) [X86Inst]
  genInstS (Call s) = return [call s]
  genInstS i = either throwError return $ genInst i

  genInst :: L1Instruction -> Either String [X86Inst]
  genInst (LabelDeclaration l) = return [declare l]
  genInst (Assign l r)         = genAssignInst l r

  genInst (MemWrite loc  (LabelL1S (Label (':':l)))) = return [
     triple "movq" (showLabel l ++ "@GOTPCREL(%rip)") (genS $ RegL1S r15)
    ,triple "movq"  (genS $ RegL1S r15) (genLoc loc)
   ]
  genInst (MemWrite loc  s) = return [triple "movq"  (genS s) (genLoc loc)]
  genInst (MathInst r op s) = return [triple (x86OpName op) (genS s) (genReg r)]
  genInst (Goto l)          = return [jumpToLabel l]
  genInst (TailCall s)      = return [jump s]
  -- special case for two numbers
  genInst (CJump (Comp (NumberL1S n1) op (NumberL1S n2)) l1 l2) =
    return $ if (cmp op n1 n2) then [jumpToLabel l1] else [jumpToLabel l2]
  -- (cjump 11 < ebx :true :false) special case. destination must be a register.
  genInst (CJump (Comp l@(NumberL1S _) op r@(RegL1S _)) l1 l2) = return [
    triple "cmpq" (genS l) (genS r),
    foldOp (jumpIfGreater l1) (jumpIfGreaterOrEqual l1) (jumpIfEqual l1) op,
    jumpToLabel l2 ]
  genInst (CJump (Comp s1 op s2) l1 l2) = return [
    triple "cmpq" (genS s2) (genS s1),
    foldOp (jumpIfLess l1) (jumpIfLessThanOrEqual l1) (jumpIfEqual l1) op,
    jumpToLabel l2 ]
  genInst Return = return ["ret"]
  genInst i = Left $ "bad instruction: " ++ show i
  
  -- several assignment cases
  genAssignInst r (SRHS (LabelL1S (Label (':':l)))) = 
    return [triple "movq" (showLabel l ++ "@GOTPCREL(%rip)") (genReg r)]
  genAssignInst r (SRHS s)            = return [triple "movq" (genS s) (genReg r)]
  genAssignInst r (MemRead loc)       = return [triple "movq" (genLoc loc) (genReg r)]
  {-
  cmp assignments have to be with CXRegisters on LHS
  (eax <- ebx < ecx)
  Here we need another trick; the x86 instruction set only let us
  update the lowest 8 bits with the result of a condition code. So,
  we do that, and then fill out the rest of the bits with zeros with
  a separate instruction:
  
    cmp %ecx, %ebx
    setl %al
    movzbl %al, %eax
  -}
  genAssignInst cx (CompRHS (Comp l@(RegL1S _)    op r@(RegL1S _))) =
    return $ genCompInst cx r l (setInstruction op)
  genAssignInst cx (CompRHS (Comp l@(NumberL1S _) op r@(RegL1S _))) =
    -- magic reverse happens here!
    return $ genCompInst cx l r (foldOp "setg" "setge" "sete" op)
  genAssignInst cx (CompRHS (Comp l@(RegL1S _)    op r@(NumberL1S _))) =
    return $ genCompInst cx r l (setInstruction op)
  genAssignInst cx (CompRHS (Comp (NumberL1S n1) op (NumberL1S n2))) =
    return [triple "movq" ("$" ++ (if (cmp op n1 n2) then "1" else "0")) (genReg cx)]
  genAssignInst Rax (Print s) = return [
    triple "movq" (genS s) (genReg rdi),
    "call _print" ]
  genAssignInst Rax (Allocate s n) = return [
    triple "movq" (genS s) (genReg rdi),
    triple "movq" (genS n) (genReg rsi),
    "call _allocate" ]
  genAssignInst Rax (ArrayError s n) = return [
    triple "movq" (genS s) (genReg rdi),
    triple "movq" (genS n) (genReg rsi),
    "call _print_error" ]
  genAssignInst l r = Left $ "bad assignment statement: " ++ show (Assign l r)
  
  genCompInst cx l r x = [
    triple "cmp" (genS l) (genS r),
    x ++ " %" ++ (low8 cx),
    triple "movzbq" ("%" ++ low8 cx) (genReg cx) ]

  declare = withLabel "declare" (++ ":")
  triple op s1 s2 = op ++ " " ++ s1 ++ ", " ++ s2
  genReg :: Register -> String
  genReg r = "%" ++ show r
  genS :: L1S -> String
  genS (RegL1S    r)  = genReg r
  genS (NumberL1S i)  = "$" ++ show i
  genS (LabelL1S  l)  = withLabel "genS" ('$':) l
  genLoc (MemLoc r i) = concat [show i, "(", genReg r, ")"]
  
  jump r@(RegL1S _)  = "jmp *" ++ genS r
  jump (LabelL1S l)  = jumpToLabel l
  jump (NumberL1S n) = error $ "bad jump to literal number: " ++ show n

  jumpToLabel :: Label -> String
  jumpToLabel = withLabel "jumpToLabel" ("jmp " ++)

  call :: L1S -> String
  call (RegL1S reg) = "call *" ++ genReg reg
  call (LabelL1S l) = callLabel l
  call x = error $ "bad call: " ++ show x

  callLabel :: Label -> String
  callLabel = withLabel "callLabel" ("call " ++)

  setInstruction = foldOp "setl" "setle" "sete"

  jumpIfLess, jumpIfLessThanOrEqual, jumpIfGreater, jumpIfGreaterOrEqual, jumpIfEqual :: Label -> String
  jumpIfLess            = withLabel "jumpIfLess" ("jl " ++)
  jumpIfLessThanOrEqual = withLabel "jumpIfLess" ("jle " ++)
  jumpIfGreater         = withLabel "jumpIfGreater" ("jg " ++)
  jumpIfGreaterOrEqual  = withLabel "jumpIfGreaterOrEqual" ("jge " ++)
  jumpIfEqual           = withLabel "jumpIfEqual" ("je " ++)

  showLabel :: String -> String
  showLabel l = (if os == Darwin then "_" else "") ++ "L1_" ++ l

  withLabel :: String -> (String -> a) -> Label -> a
  withLabel _ f (Label (':' : label)) = f (showLabel label) where
  withLabel caller _ (Label contents) = error $ "bad label: " ++ contents ++ " in: " ++ caller