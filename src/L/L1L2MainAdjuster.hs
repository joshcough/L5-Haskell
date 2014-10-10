module L.L1L2MainAdjuster (adjustMain) where

import L.L1L2AST

adjustMain :: Program x s -> Program x s
adjustMain (Program main fs) = Program (adjustMain_ main) fs
adjustMain_ :: Func x s -> Func x s
adjustMain_ (Func body) = Func $ concat [mainLabel : mainBody ++ [Return]] where
  mainLabel = LabelDeclaration ":main"
  mainBody  = stripLabel (reverse $ stripReturn $ reverse body) where
    stripLabel (LabelDeclaration ":main" : rest) = stripLabel rest
    stripLabel is = is
    stripReturn (Return : rest) = stripReturn rest
    stripReturn rest = rest