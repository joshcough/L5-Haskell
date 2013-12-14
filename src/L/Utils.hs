module L.Utils
  (
    endsWith
   ,extract
   ,mkString
   ,traceA
   ,traceSA
   ,zipWithIndex
  ) where

import Data.List
import Debug.Trace

mkString :: String -> [String] -> String
mkString s l = concat $ intersperse s l

endsWith :: String -> String -> Bool
endsWith = isSuffixOf

extract :: Either String a -> a
extract = either error id

-- TODO: really? this could be somewhere...
zipWithIndex :: [a] -> [(a, Int)]
zipWithIndex as = zip as [0..]

traceA :: Show a => a -> a
traceA a = traceShow a a

traceSA :: Show a => String -> a -> a
traceSA s a = trace (s ++ " " ++ show a) a
