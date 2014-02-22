module L.L1.L1X86 (genX86Code) where

import Control.Applicative
import Control.Monad.State
import Control.Monad.Error
import Data.List
import Data.Traversable
import L.CompilationUnit
import L.L1L2AST
import L.L1L2Parser
import L.IOHelpers
import L.Read
import L.Utils
import System.Environment 
import System.IO

-- X86 Generation code
type X86Inst = String

genX86Code :: Bool -> L1-> Either String String
genX86Code adjustStack l1 = fst $ runState (runErrorT $ genCodeS l1) 0 where
  genCodeS :: L1 -> ErrorT String (State Int) String
  genCodeS (Program main funcs) = do
    x86Funcs <- compiledFunctions
    return $ dump $ concat [mainHeader, concat x86Funcs, ["\n"]] where

    compiledFunctions :: ErrorT String (State Int) [[X86Inst]]
    compiledFunctions = Data.Traversable.sequence $ fmap compile (adjustedMain : funcs)

    adjustedMain = Func $ concat [labelMain (body main) ++ [Return]]
    labelMain is@(LabelDeclaration "main" : rest) = is
    labelMain is = LabelDeclaration "main" : is

    dump :: [X86Inst] -> String
    dump insts = mkString "\n" $ map indent insts
    indent i = if (last i == ':' || take 6 i == ".globl") then i else '\t' : i
    mainHeader = [
      ".file\t\"prog.c\"",
      ".text",
      ".globl _go",
      "_go:" ]

  compile :: L1Func -> ErrorT String (State Int) [X86Inst]
  compile f = traverse genInstS (body f') >>= return . concat where
    f' = if adjustStack then adjustStackInFunction f else f

  {-
    if we need to adjust the stack then
      -  we need to decrement the stack pointer when we enter the function
      -  whenever we return, we must make sure to put the stack pointer back.
         (by incrementing it by the amount we decremented it by 8)
         rewriteReturns puts that increment in front of all returns in the function.
   -}
  adjustStackInFunction :: L1Func -> L1Func
  adjustStackInFunction (Func (labl : insts)) =
    Func $ concat [[labl, decEsp], rewriteReturns insts] where
      decEsp = MathInst rsp decrement (NumberL1S 8)
      incEsp = MathInst rsp increment (NumberL1S 8)
      rewriteReturns insts = insts >>= f where
        f r@Return = [incEsp, r]
        f i        = [i]
  
  genInstS :: L1Instruction -> ErrorT String (State Int) [X86Inst]
  genInstS (Call s) = return [call s]
  genInstS i = either throwError return $ genInst i

  genInst :: L1Instruction -> Either String [X86Inst]
  genInst (LabelDeclaration label) = Right [declare label]
  genInst (Assign l r)       = genAssignInst l r
  genInst (MemWrite loc  s)  = Right [triple "movq"  (genS s) (genLoc loc)]
  genInst (MathInst r op s)  = Right [triple (x86OpName op) (genS s) (genReg r)]
  genInst (Goto s)           = Right [jump (LabelL1S s)]
  genInst (TailCall s)       = Right [jump s]
  -- special case for two numbers
  genInst (CJump (Comp l@(NumberL1S n1) op r@(NumberL1S n2)) l1 l2) =
    Right $ if (cmp op n1 n2) then [jump $ LabelL1S l1] else [jump $ LabelL1S l2]
  -- (cjump 11 < ebx :true :false) special case. destination must be a register.
  genInst (CJump (Comp l@(NumberL1S n) op r@(RegL1S _)) l1 l2) = Right [
    triple "cmpq" (genS l) (genS r),
    foldOp (jumpIfGreater l1) (jumpIfGreaterOrEqual l1) (jumpIfEqual l1) op,
    jump (LabelL1S l2) ]
  genInst (CJump (Comp s1 op s2) l1 l2) = Right [
    triple "cmpq" (genS s2) (genS s1),
    foldOp (jumpIfLess l1) (jumpIfLessThanOrEqual l1) (jumpIfEqual l1) op,
    jump (LabelL1S l2) ]
  genInst Return = Right ["ret"]
  genInst i = Left $ "bad instruction: " ++ show i
  
  -- several assignment cases
  genAssignInst r (SRHS s)      = Right [triple "movq" (genS s) (genReg r)]
  genAssignInst r (MemRead loc) = Right [triple "movq" (genLoc loc) (genReg r)]
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
    Right $ genCompInst cx r l (setInstruction op)
  genAssignInst cx (CompRHS (Comp l@(NumberL1S _) op r@(RegL1S _))) =
    -- magic reverse happens here!
    Right $ genCompInst cx l r (foldOp "setg" "setge" "sete" op)
  genAssignInst cx (CompRHS (Comp l@(RegL1S _)    op r@(NumberL1S _))) =
    Right $ genCompInst cx r l (setInstruction op)
  genAssignInst cx (CompRHS (Comp l@(NumberL1S n1) op r@(NumberL1S n2))) =
    Right [triple "movq" ("$" ++ (if (cmp op n1 n2) then "1" else "0")) (genReg cx)]
  genAssignInst rax (Print s) = Right [
    triple "movq" (genS s) (genReg rdi),
    "call _print" ]
  genAssignInst rax (Allocate s n) = Right [
    triple "movq" (genS s) (genReg rdi),
    triple "movq" (genS n) (genReg rsi),
    "call _allocate" ]
  genAssignInst rax (ArrayError s n) = Right [
    triple "movq" (genS s) (genReg rdi),
    triple "movq" (genS n) (genReg rsi),
    "call _print_error" ]
  genAssignInst l r = Left $ "bad assignment statement: " ++ show (Assign l r)
  
  genCompInst cx l r x = [
    triple "cmp" (genS l) (genS r),
    x ++ " " ++ (low8 cx),
    triple "movzbq" (low8 cx) (genReg cx) ]
    where low8 cx = "%" ++ [show cx !! 1] ++ "l"
  
  declare label = "L1_" ++ label ++ ":"
  triple op s1 s2 = op ++ " " ++ s1 ++ ", " ++ s2
  genReg :: Register -> String
  genReg r = "%" ++ (show r)
  genS :: L1S -> String
  genS (NumberL1S i) = "$" ++ show i
  genS (LabelL1S  l) = "$L1_" ++ l
  genS (RegL1S    r) = genReg r
  genLoc (MemLoc r i) = concat [show i, "(", genReg r, ")"]
  
  jump :: L1S -> String
  jump (LabelL1S name) = "jmp L1_" ++ name
  jump l               = "jmp *" ++ (genS l)

  call :: L1S -> String
  call (LabelL1S name) = "call L1_" ++ name
  call x = error $ "bad call: " ++ show x
  
  setInstruction = foldOp "setl" "setle" "sete"
  
  jumpIfLess            l = "jl L1_"  ++ l
  jumpIfLessThanOrEqual l = "jle L1_" ++ l
  jumpIfGreater         l = "jg L1_"  ++ l
  jumpIfGreaterOrEqual  l = "jge L1_" ++ l
  jumpIfEqual           l = "je L1_"  ++ l
