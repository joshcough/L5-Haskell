module L.L3.L3 where

import Control.Applicative
import L.L1L2AST  as L2
import L.L3.L3AST as L3
import L.L3.L3Parser

todo = error
-- TODO: both of these are also in other code.
mainLabel = "__main__"

argumentRegisters :: [Register]
argumentRegisters = [rdi, rsi, rdx, rcx, r8, r9]
toSRHS = SRHS . XL2S . RegL2X

compile :: L3 -> L2
compile (L3.Program e funcs) = L2.Program callMain l2Funcs where
  callMain :: L2.L2Func
  callMain = L2.Func [L2.Call $ LabelL2S mainLabel]
  l2Funcs :: [L2.L2Func]
  l2Funcs  = L2.Func (compileE e) : fmap compileFunction funcs 

-- (l (x ...) e)
-- there is an implicit assertion here that L3 functions
-- can have at most 6 arguments,
-- because there are 6 x86-64 argument registers
compileFunction :: L3.Func -> L2.L2Func 
compileFunction (L3.Func name args body) = L2.Func insts where
  labl = LabelDeclaration name
  as :: [(L2X, AssignRHS L2X L2S)]
  as = zip (fmap VarL2X args) (fmap toSRHS argumentRegisters)
  argAssignments :: [L2Instruction]
  argAssignments = fmap (uncurry Assign) as
  insts :: [L2Instruction]
  insts = concat [[labl], argAssignments, compileE body, [Return]]

-- e ::= (let ([x d]) e) | (if v e e) | d
compileE :: E -> [L2Instruction]
compileE (Let v d e) = compileD d (VarL2X v) ++ compileE e
compileE (IfStatement v t f) = todo "need state!"
compileE _ = todo "more cases"

compileD :: D -> L2X -> [L2Instruction]
compileD d dest = todo

{-
def compileE(e:E): List[L2Instruction] = e match {
    case Let(v:Variable, d:D, body:E) => compileD(d=d, destination=v) ::: compileE(body)
    case IfStatement(v:V, t:E, f:E) => {
      val tmp = temp()
      val thenLabel = tempLabel()
      val elseLabel = tempLabel()
      List(
        compileD(v, tmp),
        List(CJump(Comp(tmp, L2EqualTo, Num(1)), elseLabel, thenLabel)),
        List(LabelDeclaration(thenLabel)),
        compileE(t),
        List(LabelDeclaration(elseLabel)),
        compileE(f)).flatten
    }

    //3) the e is a d:
    //-> if it is an application, make a tail call
    // otherwise, generate the code for the d,
    // store the result in eax, and return.
    // TODO: something is funny about passing eax and tailcall=true...
    // it seems like i can collapse that into one argument somehow.
    // because eax sort of signals that a tailcall should happen.
    // otherwise we would get a variable and we need to store
    // that variable into eax...hmmm...
    case f:FunCall => compileFunCall(f, eax, tailCall = true)
    case d:D => compileD(d, eax) ::: List(Return)
  }
-}
