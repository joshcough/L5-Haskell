module L.L1.MainAdjuster (adjustMain) where

import L.L1L2AST

adjustMain :: L1 -> L1
adjustMain (Program main fs) = Program (adjustMain_ main) fs
adjustMain_ :: L1Func -> L1Func
adjustMain_ (Func body) = Func $ concat [mainLabel : mainBody ++ [Return]] where
  mainLabel = LabelDeclaration ":main"
  mainBody = stripLabel (reverse $ stripReturn $ reverse body) where
    stripLabel (LabelDeclaration ":main" : rest) = stripLabel rest
    stripLabel is = is
    stripReturn (Return : rest) = stripReturn rest
    stripReturn rest = rest
