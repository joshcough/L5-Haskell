module L.Read 
  (
    SExpr(..)
   ,debugShowSExpr
   ,flatten
   ,showAsList
   ,sreadWithRest
   ,sread
  ) where

import Data.List
import Data.Char (isSpace)
import L.Utils

trim :: String -> String
trim = f . f where f = reverse . dropWhile isSpace

data SExpr = AtomSym String | AtomNum Int | AtomChar Char | List [SExpr] deriving (Eq)

instance Show SExpr where
  show (AtomSym s)  = s
  show (AtomNum i)  = show i
  show (AtomChar c) = show c
  show (List exps)  = showAsList $ fmap show exps

debugShowSExpr :: SExpr -> String
debugShowSExpr (AtomSym s)   = wrap "symbol" s
debugShowSExpr (AtomNum i)   = wrap "number" i
debugShowSExpr (AtomChar c)  = wrap "char"   c
debugShowSExpr l@(List exps) = wrap "list"   (fmap debugShowSExpr exps)

wrap typ dat = concat ["{", typ, ": ", show dat, "}"]

showAsList :: [String] -> String
showAsList as = "(" ++ (mkString " " as) ++ ")"

sread :: String -> SExpr
sread s = let (sexpr, _) = readWithRest (preprocess s) in sexpr

sreadWithRest :: String -> (SExpr, String)
sreadWithRest = readWithRest . preprocess 

preprocess :: String -> String
preprocess s = concat $ map ((++ " ") . trim . removeComments) (lines s)
removeComments :: String -> String
removeComments s = takeWhile (not . (==';')) s

readL :: String -> SExpr -> (SExpr,String)
readL (')' : tail) (List acc) = (List acc, tail)
readL (' ' : tail) acc = readL tail acc
readL (x : xs) (List acc) = let (next, rest) = readWithRest(x : xs) in readL rest (List (acc ++ [next]))
readL _ _ = error "unterminated list"

ends = [' ', ')', ']']
readChars :: String -> String -> (SExpr, String)
readChars (c : tail) acc
  | elem c ends = (symOrNum acc, c : tail)  
  | otherwise   = readChars tail (acc ++ [c]) 
readChars [] acc = (symOrNum acc, [])

symOrNum :: String -> SExpr
symOrNum s = if isInt s then AtomNum (read s :: Int) else AtomSym s

readWithRest :: String -> (SExpr,String)
readWithRest (' ' : tail) = readWithRest tail
readWithRest ('(' : tail) = readL tail (List [])
--readWithRest ('"' : tail) = readStringLit tail ['"']
readWithRest ('\'' : c : '\'' : tail) = (AtomChar c, tail)
readWithRest (c : tail) = readChars (c : tail) []

--readStringLit :: String -> String -> (SExpr, String)
--readStringLit ('"' : tail) acc = (StringLit (acc ++ ['"']), tail)
--readStringLit (c : tail) acc = readStringLit tail (acc ++ [c])

isInt :: String -> Bool
isInt s = case reads s :: [(Int, String)] of
  [(_, "")] -> True
  _         -> False

flatten :: SExpr -> [String]
flatten (AtomSym s) = [s]
flatten (AtomNum n) = [show n]
flatten (List ss) = ss >>= flatten

--------------------
------ tests -------
--------------------
{--
results = runTests
  [
    makeTest "7" (sread "7") (AtomNum 7),
    makeTest "x" (sread "x") (AtomSym "x"),
    makeTest "(7)" (sread ("(7)")) (List [AtomNum 7]),
    makeTest "(+ 5 6)" (sread "(+ 5 6)") (List [AtomSym "+", AtomNum 5, AtomNum 6]),
    makeTest "(+ 5 6)" (sread "(+ (+ 5 6) 6)") (List [AtomSym "+", (List [AtomSym "+", AtomNum 5, AtomNum 6]), AtomNum 6]),
    makeTest "(+ 5 6)" (sread "(+ (+ 5 6) (+ 5 6))") (List [AtomSym "+", (List [AtomSym "+", AtomNum 5, AtomNum 6]), (List [AtomSym "+", AtomNum 5, AtomNum 6])]),
    makeTest "(f x)" (sread "(f x)") (List [AtomSym "f", AtomSym "x"])
  ]
--}
