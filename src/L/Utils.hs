module L.Utils
  (
    endsWith
   ,extract
   ,mkString
   ,zipWithIndex
  ) where

import Data.List

mkString :: String -> [String] -> String
mkString s l = concat $ intersperse s l

endsWith :: String -> String -> Bool
endsWith = isSuffixOf

extract :: Either String a -> a
extract = either error id

-- TODO: really? this could be somewhere...
zipWithIndex :: [a] -> [(a, Int)]
zipWithIndex as = zip as [0..]
