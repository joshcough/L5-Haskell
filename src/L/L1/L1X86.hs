module L.L1.L1X86
  (
    genX86Code
  ) where

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

genX86Code :: L1 -> Either String String
genX86Code l1 = fst $ runState (runErrorT $ genCodeS l1) 0 where
  genCodeS :: L1 -> ErrorT String (State Int) String
  genCodeS (Program main funcs) = do
    x86Main  <- genMain main
    x86Funcs <- genFunc $ concat $ map body funcs
    return $ dump $ concat [x86Main, x86Funcs, ["\n"]] where
    dump :: [X86Inst] -> String
    dump insts = mkString "\n" $ map adjust insts
    adjust i = if (last i == ':' || take 6 i == ".globl") then i else '\t' : i

  genMain :: L1Func -> ErrorT String (State Int) [X86Inst]
  genMain (Func insts) =  genFunc_ (tail insts) mainHeader mainFooter where
    mainHeader = [
      ".file\t\"prog.c\"",
      ".text",
      ".globl _go",
      "_go:",
      "subq $8, %rsp" ]
    mainFooter = [
      "addq $8, %rsp",
      "ret" ]

  genFunc :: [L1Instruction] -> ErrorT String (State Int) [X86Inst]
  genFunc [] = return []
  genFunc insts = do
    labl <- genInstS (head insts)
    body <- compile  (tail insts)
    return $ concat [labl, header, body] where
      header = [
        "pushq %rbp",
        "movq %rsp, %rbp" ]
 
  genFunc_ :: [L1Instruction] -> [String] -> [String] -> ErrorT String (State Int) [X86Inst]
  genFunc_ insts header footer = wrap header footer <$> (compile insts) where
    wrap header footer body = concat [header, body, footer]

  compile :: [L1Instruction] -> ErrorT String (State Int) [X86Inst]
  compile insts = traverse genInstS insts >>= return . concat
  
  genInstS :: L1Instruction -> ErrorT String (State Int) [X86Inst]
  genInstS (Call s) = return [call s]
  genInstS i = either throwError return $ genInst i

  genInst :: L1Instruction -> Either String [X86Inst]
  genInst (LabelDeclaration label)     = Right [declare label]
  genInst (Assign l r)       = genAssignInst l r
  genInst (MemWrite loc  s)  = Right [triple "movq"  (genS s) (genLoc loc)]
  genInst (MathInst r op s)  = Right [triple (x86OpName op) (genS s) (genReg r)]
  genInst (Goto s)           = Right [jump (LabelL1S s)]
  genInst (TailCall s)       = Right ["movq %rbp, %rsp", jump s]
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
  genInst Return = Right [
    "leave",
    "ret" ]
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
  --genS (RegL1S    r) = "%" ++ (show r)
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
