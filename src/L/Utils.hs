module L.Utils
  (
    extract
   ,mkString
   ,zipWithIndex
  ) where

import Data.List

mkString :: String -> [String] -> String
mkString s l = concat $ intersperse s l

extract :: Either String a -> a
extract = either error id

-- TODO: really? this could be somewhere...
zipWithIndex :: [a] -> [(a, Int)]
zipWithIndex as = zip as [0..]
