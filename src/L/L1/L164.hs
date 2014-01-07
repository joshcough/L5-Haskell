module L.L1.L164
  (
    genX8664Code
  ) where

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
type X8664Inst = String

genX8664Code :: L1 -> Either String String
genX8664Code l1 = fst $ runState (runErrorT $ genCodeS l1) 0 where
  genCodeS :: L1 -> ErrorT String (State Int) String
  genCodeS (Program main funcs) = do
    x86Main  <- genMain main
    x86Funcs <- genFunc $ concat $ map body funcs
    return $ dump $ concat [header, x86Main, x86Funcs] where
    dump :: [X8664Inst] -> String
    dump insts = mkString "\n" $ map adjust insts
    adjust i = if (last i == ':' || take 6 i == ".globl") then i else '\t' : i
    header = [
      ".file\t\"prog.c\"",
      ".text",
      ".globl go",
      "_go:",
      "pushq %rbp",
      "movq %rsp, %rbp",
      "pushq %rbx",
      "pushq %rsi",
      "pushq %rdi",
      "pushq %rbp" ]

  genMain :: L1Func -> ErrorT String (State Int) [X8664Inst]
  genMain (Func insts) =  (flip fmap) (genFunc (tail insts)) (++ mainFooter) where
    mainFooter = [
      "popq %rbp",
      "popq %rdi",
      "popq %rsi",
      "popq %rbx",
      "leave",
      "ret" ]

  genFunc :: [L1Instruction] -> ErrorT String (State Int) [X8664Inst]
  genFunc insts = (traverse genInstS insts) >>= return . concat
  
  genInstS :: L1Instruction -> ErrorT String (State Int) [X8664Inst]
  genInstS (Call s) = fmap (\i -> call $ "Generated_Label_" ++ show i) postIncrement where
    postIncrement = do { x <- get; put (x+1); return x }
    call label = [
      "pushl " ++ (genS $ LabelL1S label),
      "pushl %ebp",
      "movl %esp, %ebp",
      jump s,
      declare label ]
  genInstS i = either throwError return $ genInst i

  genInst :: L1Instruction -> Either String [X8664Inst]
  genInst (LabelDeclaration label)     = Right [declare label]
  genInst (Assign l r)       = genAssignInst l r
  genInst (MemWrite loc  s)  = Right [triple "movl"  (genS s) (genLoc loc)]
  genInst (MathInst r op s)  = Right [triple (x86OpName op) (genS s) (genReg r)]
  genInst (Goto s)           = Right [jump (LabelL1S s)]
  genInst (TailCall s)       = Right ["movl %ebp, %esp", jump s]
  -- special case for two numbers
  genInst (CJump (Comp l@(NumberL1S n1) op r@(NumberL1S n2)) l1 l2) =
    Right $ if (cmp op n1 n2) then [jump $ LabelL1S l1] else [jump $ LabelL1S l2]
  -- (cjump 11 < ebx :true :false) special case. destination must be a register.
  genInst (CJump (Comp l@(NumberL1S n) op r@(RegL1S _)) l1 l2) = Right [
    triple "cmpl" (genS l) (genS r),
    foldOp (jumpIfGreater l1) (jumpIfGreaterOrEqual l1) (jumpIfEqual l1) op,
    jump (LabelL1S l2) ]
  genInst (CJump (Comp s1 op s2) l1 l2) = Right [
    triple "cmpl" (genS s2) (genS s1),
    foldOp (jumpIfLess l1) (jumpIfLessThanOrEqual l1) (jumpIfEqual l1) op,
    jump (LabelL1S l2) ]
  genInst Return = Right [
    "movl %ebp, %esp",
    "popl %ebp",
    "ret" ]
  genInst i = Left $ "bad instruction: " ++ show i
  
  -- several assignment cases
  genAssignInst r (SRHS s)      = Right [triple "movl" (genS s) (genReg r)]
  genAssignInst r (MemRead loc) = Right [triple "movl" (genLoc loc) (genReg r)]
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
  genAssignInst cx@(CXR c) (CompRHS (Comp l@(RegL1S _)    op r@(RegL1S _))) =
    Right $ genCompInst cx r l (setInstruction op)
  genAssignInst cx@(CXR c) (CompRHS (Comp l@(NumberL1S _) op r@(RegL1S _))) =
    -- magic reverse happens here!
    Right $ genCompInst cx l r (foldOp "setg" "setge" "sete" op)
  genAssignInst cx@(CXR c) (CompRHS (Comp l@(RegL1S _)    op r@(NumberL1S _))) =
    Right $ genCompInst cx r l (setInstruction op)
  genAssignInst cx@(CXR _) (CompRHS (Comp l@(NumberL1S n1) op r@(NumberL1S n2))) =
    Right [triple "movl" ("$" ++ (if (cmp op n1 n2) then "1" else "0")) (genReg cx)]
  genAssignInst (CXR Eax) (Print s) = Right [
    triple "movl" (genS s) (genReg edi),
    "call print" ]
  genAssignInst (CXR Eax) (Allocate s n) = Right [
    triple "movl" (genS s) (genReg edi),
    triple "movl" (genS n) (genReg esi),
    "call allocate" ]
  genAssignInst (CXR Eax) (ArrayError s n) = Right [
    triple "movl" (genS s) (genReg edi),
    triple "movl" (genS n) (genReg esi),
    "call print_error" ]
  genAssignInst l r = Left $ "bad assignment statement: " ++ show (Assign l r)
  
  genCompInst cx@(CXR c) l r x = [
    triple "cmp" (genS l) (genS r),
    x ++ " " ++ (low8 c),
    triple "movzbl" (low8 c) (genReg cx) ]
    where low8 cx = "%" ++ [show cx !! 1] ++ "l"
  
  declare label = "L1_" ++ label ++ ":"
  triple op s1 s2 = op ++ " " ++ s1 ++ ", " ++ s2
  genReg :: Register -> String
  genReg (CXR cx) = "%" ++ show cx
  genReg (XR x)   = "%" ++ show x
  genS :: L1S -> String
  genS (NumberL1S i) = "$" ++ show i
  genS (LabelL1S  l) = "$L1_" ++ l
  genS (RegL1S    r) = genReg r
  --genS (RegL1S    r) = "%" ++ (show r)
  genLoc (MemLoc r i) = concat [show i, "(", genReg r, ")"]
  
  jump :: L1S -> String
  jump (LabelL1S name) = "jmp L1_" ++ name
  jump l               = "jmp *" ++ (genS l)
  
  setInstruction = foldOp "setl" "setle" "sete"
  
  jumpIfLess            l = "jl L1_"  ++ l
  jumpIfLessThanOrEqual l = "jle L1_" ++ l
  jumpIfGreater         l = "jg L1_"  ++ l
  jumpIfGreaterOrEqual  l = "jge L1_" ++ l
  jumpIfEqual           l = "je L1_"  ++ l
