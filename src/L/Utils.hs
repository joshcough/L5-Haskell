module L.Utils
  (
    (|>)
   ,endsWith
   ,extract
   ,mkString
   ,strip
   ,traceA
   ,traceSA
   ,zipWithIndex
  ) where

import Data.List
import Debug.Trace

(|>) :: (a -> b) -> (b -> c) -> a -> c
(|>) = flip (.)

mkString :: String -> [String] -> String
mkString s l = concat $ intersperse s l

endsWith :: String -> String -> Bool
endsWith = isSuffixOf

extract :: Either String a -> a
extract = either error id

zipWithIndex :: [a] -> [(a, Int)]
zipWithIndex as = zip as [0..]

traceA :: Show a => a -> a
traceA a = traceShow a a

traceSA :: Show a => String -> a -> a
traceSA s a = trace (s ++ " " ++ show a) a

wschars :: String
wschars = " \t\r\n"

strip :: String -> String
strip = lstrip . rstrip

lstrip :: String -> String
lstrip s = case s of
  [] -> []
  (x:xs) -> if elem x wschars then lstrip xs else s

rstrip :: String -> String
rstrip = reverse . lstrip . reverse
