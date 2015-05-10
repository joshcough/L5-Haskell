module L.L1.L1L2MainAdjuster (adjustMain) where

import L.L1.L1L2AST
import L.Primitives (Label(..))

adjustMain :: Program x s -> Program x s
adjustMain (Program main fs) = Program (adjustMain_ main) fs
adjustMain_ :: Func x s -> Func x s
adjustMain_ (Func body) = Func $ concat [mainLabel : mainBody ++ [Return]] where
  mainLabel = LabelDeclaration $ Label ":main"
  mainBody  = stripLabel (reverse $ stripReturn $ reverse body) where
    stripLabel (LabelDeclaration (Label ":main") : rest) = stripLabel rest
    stripLabel is = is
    stripReturn (Return : rest) = stripReturn rest
    stripReturn rest = rest
