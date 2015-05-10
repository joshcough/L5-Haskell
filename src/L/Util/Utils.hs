module L.Util.Utils
  (
    (|>)
   ,bind2
   ,bind3
   ,endsWith
   ,extract
   ,mkString
   ,strip
   ,zipWithIndex
  ) where

import Data.List

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

bind2 :: Monad m => (a -> b -> m c) -> m a -> m b -> m c
bind2 f ma mb = do a <- ma; b <- mb; f a b

bind3 :: Monad m => (a -> b -> c -> m d) -> m a -> m b -> m c -> m d
bind3 f ma mb mc = do a <- ma; b <- mb; c <- mc; f a b c