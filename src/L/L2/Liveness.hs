{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module L.L2.Liveness
  (
    InstructionInOutSet(..)
   ,IIOS
   ,liveness
   ,livenessMain
   ,livenessMain_
   ,runLiveness
   ,showLiveness
  ) where

import Control.Monad
import Data.List
import Data.Maybe
import qualified Data.Map as M
import qualified Data.Set as S
import L.CompilationUnit
import L.L1L2AST
import L.L1L2Parser
import L.Read
import L.Utils

data InstructionInOutSet = InstructionInOutSet {
  index  :: Int,       inst    :: L2Instruction,
  genSet :: S.Set L2X, killSet :: S.Set L2X,
  inSet  :: S.Set L2X, outSet  :: S.Set L2X
} deriving (Eq)

type IIOS = InstructionInOutSet

showLiveness :: [IIOS] -> String
showLiveness is = 
  let go :: (IIOS -> S.Set L2X) -> String
      go f = mkString " " $ fmap showAsList (fmap (S.toList . f) is) 
  in "((in " ++ go inSet ++ ") (out " ++ go outSet ++ "))" 

set :: AsL2X x => [x] -> S.Set L2X
set = S.fromList . fmap asL2X

callerSave     = set [eax, ebx, ecx, edx]
x86CallerSave  = set [eax, ecx, edx]
calleeSave     = set [edi, esi]
arguments      = set [eax, ecx, edx]
resultRegister = set [eax]

gen :: L2Instruction -> S.Set L2X
gen i = genI i

genI :: L2Instruction -> S.Set L2X
genI (Assign x rhs)             = genRHS rhs
genI (MathInst x _ s)           = S.unions [genX x,  genS s]
genI (MemWrite (MemLoc bp _) s) = S.unions [genX bp, genS s]
genI (Goto s)                   = S.empty
genI (CJump (Comp s1 _ s2) _ _) = S.unions [genS s1, genS s2]
genI (LabelDeclaration _)       = S.empty
genI (Call s)                   = S.unions [genS s,  arguments]
genI (TailCall s)               = S.unions [genS s,  arguments, calleeSave]
genI Return                     = S.unions [resultRegister, calleeSave]

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
kill (Call s)                     = S.unions [callerSave, resultRegister]
kill _                            = S.empty

liveness :: [L2Instruction] -> [IIOS]
liveness = head . inout

runLiveness :: String -> [IIOS]
runLiveness = liveness . extract . parseL2InstList . sread

livenessMain_ :: FilePath -> IO (CompilationUnit [IIOS])
livenessMain_ = compile1 runLiveness "lres"

livenessMain :: IO ()
livenessMain = compile (showLiveness . runLiveness) "lres"

-- builds up a giant list of all the intermediate inout results
-- robby starts out with a function, and empty in and out sets for each instruction
-- that is the first result in the list return here
-- then there is a result for each slide all the way down to the last slide
-- when things are finally complete (we've reached the fixed point)
inout :: [L2Instruction] -> [[IIOS]]
inout is =
  let 
      nrInstructions :: Int
      nrInstructions = length is
      instructionsWithIndex :: [(L2Instruction, Int)]
      instructionsWithIndex = zipWithIndex is
      -- TODO: i think this map is busted because there could be two equal instructions
      -- it's only used to find label declarations too... this should be easier.
      indeces :: M.Map L2Instruction Int
      indeces = M.fromList instructionsWithIndex
      findLabelDecIndex :: Label -> Int
      findLabelDecIndex l = 
        maybe (error $ "no such label: " ++ l) id (M.lookup (LabelDeclaration l) indeces) 
      succIndeces :: [S.Set Int]
      succIndeces = fmap succIndeces_ instructionsWithIndex where
        succIndeces_ :: (L2Instruction, Int) -> S.Set Int
        succIndeces_ (i, n) = case i of
          Return                    -> S.empty
          TailCall _                -> S.empty
          Assign _ (ArrayError _ _) -> S.empty
          Goto label                -> S.singleton $ findLabelDecIndex label
          CJump _ l1 l2             -> S.fromList [findLabelDecIndex l1, findLabelDecIndex l2]
          -- we have to test that there is something after this instruction
          -- in case the last instruction is something other than return or cjump
          -- i think that in normal functions this doesn't happen but the hw allows it.
          _ -> if (nrInstructions > (n + 1)) then  S.singleton (n+1) else S.empty
      step :: [IIOS] -> [IIOS]
      step current = fmap step_ current where
        step_ :: IIOS -> IIOS
        step_ i =
              -- in(n) = gen(n-th-inst) ∪ (out (n) - kill(n-th-inst))
          let newIn :: S.Set L2X
              newIn = S.union (genSet i) ((outSet i) S.\\ (killSet i))
              -- out(n) = ∪{in(m) | m ∈ succ(n)}
              newOut :: S.Set L2X
              newOut = S.fromList $ (fmap (current !!) $ S.toList (succIndeces !! (index i))) >>= S.toList . inSet
          in InstructionInOutSet (index i) (inst i) (genSet i) (killSet i) newIn newOut
      -- does the next round of moving things up the in/out chains.
      -- recurs until the result is the same as what we've got so far.
      inout_ :: [[IIOS]] -> [[IIOS]]
      inout_ acc =
        let current = acc !! 0 --trace (showLiveness $ acc !! 0) $ acc !! 0
            nextStep = step current
        in if (nextStep == current) then acc else inout_ (nextStep : acc)
      -- start out with empty in and out sets for all instructions
      emptyStartSet :: [IIOS]
      emptyStartSet = fmap f instructionsWithIndex where
        f (inst, index) = InstructionInOutSet index inst (gen inst) (kill inst) S.empty S.empty
  -- then fill them in until we reach the fixed point.
  in inout_ [emptyStartSet]

{--
  // just gets the last inout result. (the most important one)
  def inoutFinalResult(f:Func): List[InstructionInOutSet] = inout(f.body).head

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
