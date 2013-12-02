{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module L.L2.Liveness where

import Control.Monad
import Data.Maybe
import qualified Data.Map as M
import qualified Data.Set as S
import L.L1L2AST

data InstructionInOutSet = InstructionInOutSet {
  index  :: Int,       inst    :: L2Instruction,
  genSet :: S.Set L2X, killSet :: S.Set L2X,
  inSet  :: S.Set L2X, outSet  :: S.Set L2X
}

set :: AsL2X x => [x] -> S.Set L2X
set = S.fromList . fmap asL2X

callerSave    = set [eax, ebx, ecx, edx]
x86CallerSave = set [eax, ecx, edx]
calleeSave    = set [edi, esi]
arguments     = set [eax, ecx, edx]
result        = set [eax]

gen :: L2Instruction -> S.Set L2X
gen i = genI i where
  genI :: L2Instruction -> S.Set L2X
  genI (Assign x rhs)             = genRHS rhs
  genI (MathInst x _ s)           = S.unions [genX x,  genS s]
  genI (MemWrite (MemLoc bp _) s) = S.unions [genX bp, genS s]
  genI (Goto s)                   = S.empty
  genI (CJump (Comp s1 _ s2) _ _) = S.unions [genS s1, genS s2]
  genI (LabelDeclaration _)       = S.empty
  genI (Call s)                   = S.unions [genS s,  arguments]
  genI (TailCall s)               = S.unions [genS s,  arguments, calleeSave]
  genI Return                     = S.unions [result, calleeSave]

  genX :: L2X -> S.Set L2X
  genX (RegL2X r) = set [r]
  genX (VarL2X v) = set [v]

  genS :: L2S -> S.Set L2X
  genS (XL2S x)        = genX x
  genS (NumberL2S n)   = S.empty
  genS (LabelL2S l)    = S.empty
  
  genRHS :: AssignRHS L2X L2S -> S.Set L2X
  genRHS (CompRHS (Comp s1 _ s2)) = S.unions [genS s1, genS s2]
  genRHS (Allocate n init)        = S.unions [genS n,  genS init]
  genRHS (Print s)                = genS s
  genRHS (ArrayError a n)         = S.unions [genS a,  genS n]
  genRHS (MemRead (MemLoc bp _))  = genX bp
  genRHS (SRHS s)                 = genS s

kill :: L2Instruction -> S.Set L2X
kill (Assign x (Print s))         = S.insert x x86CallerSave
kill (Assign x (Allocate a init)) = S.insert x x86CallerSave
kill (Assign x (ArrayError a n))  = S.insert x x86CallerSave
kill (Assign x _)                 = set [x]
kill (MathInst x _ _)             = set [x]
kill (Call s)                     = S.unions [callerSave, result]
kill _                            = S.empty

{--

  // just gets the last inout result. (the most important one)
  def inoutFinalResult(f:Func): List[InstructionInOutSet] = inout(f.body).head

  // builds up a giant list of all the intermediate inout results
  // robby starts out with a function, and empty in and out sets for each instruction
  // that is the first result in the list return here
  // then there is a result for each slide all the way down to the last slide
  // when things are finally complete (we've reached the fixed point)
  def inout(instructions:List[Instruction]): List[List[InstructionInOutSet]] = {
    val instructionsWithIndex = instructions.zipWithIndex
    val indeces = instructionsWithIndex.toMap
    def findLabelDecIndex(label: Label): Int =
      indeces.getOrElse(LabelDeclaration(label), error("no such label: " + label.name))
    val succIndeces = instructionsWithIndex.map {
      case (i, n) => i match {
        case Return | TailCall(_) | Assignment(_, ArrayError(_, _)) => Set()
        case Goto(label) => Set(findLabelDecIndex(label))
        case CJump(_, l1, l2) => Set(findLabelDecIndex(l1), findLabelDecIndex(l2))
        // we have to test that there is something after this instruction
        // in case the last instruction is something other than return or cjump
        // i think that in normal functions this doesn't happen but the hw allows it.
        case _ if (instructions.isDefinedAt(n + 1)) => Set(n + 1)
        case _ => Set()
      }
    }.toIndexedSeq

    // does the next round of moving things up the in/out chains.
    // recurs until the result is the same as what we've got so far.
    def inout(acc:List[List[InstructionInOutSet]]): List[List[InstructionInOutSet]] = {
      val current = acc.head
      var changes: Boolean = false
      // build the next result
      val nextStep = current.map(i => {
        // in(n) = gen(n-th-inst) ∪ (out (n) - kill(n-th-inst))
        val newIn = i.gen union (i.out -- i.kill)
        // out(n) = ∪{in(m) | m ∈ succ(n)}
        val newOut = succIndeces(i.index).map(current(_)).flatMap(_.in)
        if(newIn.size > i.in.size || newOut.size > i.out.size) changes = true
        i.copy(in=newIn, out=newOut)
      })
      // if we've reached the fixed point, we can stop. otherwise continue.
      // if(nextStep == current) acc else inout(nextStep :: acc)
      if(!changes) acc else inout(nextStep :: acc)
    }

    // start out with empty in and out sets for all instructions
    val emptyStartSet = instructionsWithIndex.map {
      case (inst, index) => InstructionInOutSet(index, inst, gen(inst), kill(inst), Set[X](), Set[X]())
    }
    // then fill them in until we reach the fixed point.
    inout(List(emptyStartSet))
  }

  def liveRanges(iioss: List[InstructionInOutSet]): List[List[LiveRange]] = {
    def liveRanges(x: X, sets: List[List[X]]): List[LiveRange] = sets match {
      case Nil => Nil
      case y::ys => {
        if(y contains x) {
          val (h,t) = sets.span(_.contains(x))
          LiveRange(x, h.size) :: liveRanges (x, t)
        } else liveRanges(x, sets.dropWhile(! _.contains(x)))
      }
    }
    val inSets = iioss.map(_.in)
    val variablesAndRegisters = inSets.foldLeft(Set[X]()){
      case (acc, s) => acc union s
    }.filterNot(x => x == ebp || x == esp)
    for(x <- variablesAndRegisters.toList.sorted) yield liveRanges(x, inSets.map(_.toList))
  }
}
--}

{--


// TODO: probably should fill in the usages variable.
case class LiveRange(x:X, range:Int, usages:Int=0) extends Ordered[LiveRange] {
  def compare(that: LiveRange) = this.range compare that.range
}


import L2AST._
import util.Timer

object LivenessMain {
  import io.FileHelper._
  import java.io.File
  import L2CompilerMain._

  def main(args:Array[String]){ println(liveness(new File(args(0)).read)) }

  //  % liveness f.L2f
  //  ((in (eax) (eax x)) (out (eax x) ()))
  def liveness(code:String) = L2Printer.hwView(inoutForTesting(code))
--}
