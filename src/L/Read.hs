module L.Read 
  (
    ParseResult
   ,SExpr(..)
   ,flatten
   ,liftParser
   ,runParser
   ,runParserOrDie
   ,intParser
   ,sexprParser
   ,showAsList
   ,sreadWithRest
   ,sread
   ,trim
  ) where

import Control.Applicative
import Control.Monad
import Data.ByteString.UTF8 as UTF8 hiding (lines)
import Data.Char (isDigit, isSpace)
import qualified Data.HashSet as HashSet
import Data.HashSet (HashSet)
import Data.List
import Data.Semigroup
import Text.Parser.Combinators
import Text.Parser.LookAhead
import Text.Parser.Token
import qualified Text.Parser.Token.Highlight as Highlight
import Text.Read (readMaybe)
import Text.Trifecta hiding (semi)
import qualified Text.Trifecta as Trifecta (semi)
import Text.Trifecta.Parser
import Text.Trifecta.Result

import L.Utils

type ParseResult a = Either String a

trim :: String -> String
trim = f . f where f = reverse . dropWhile isSpace

data SExpr = AtomSym String | AtomNum Int | List [SExpr] deriving (Eq)

instance Show SExpr where
  show (AtomSym s) = s
  show (AtomNum i) = show i
  show (List exps) = showAsList $ fmap show exps

showAsList :: [String] -> String
showAsList as = "(" ++ (mkString " " as) ++ ")"

sread :: String -> SExpr
sread = runParserOrDie sexprParser . preprocess

sreadWithRest :: String -> (SExpr, String)
sreadWithRest = runParserOrDie ((,) <$> sexprParser <*> many anyChar) . preprocess

preprocess :: String -> String
preprocess = trim . concat . map f . lines where
  f = (++ " ") . trim . removeComments
  removeComments = takeWhile $ not . (==';')

flatten :: SExpr -> [String]
flatten (AtomSym s) = [s]
flatten (AtomNum n) = [show n]
flatten (List ss)   = ss >>= flatten

liftParser :: (SExpr -> ParseResult p) -> String -> ParseResult p
liftParser f = f . sread

sexprParser :: Parser SExpr
sexprParser = choice [intParser, symParser, listParser] 

variable :: TokenParsing m => IdentifierStyle m
variable = IdentifierStyle
  { _styleName = "token"
  , _styleStart = varInit
  , _styleLetter = varSubsequent
  , _styleReserved = HashSet.empty
  , _styleHighlight = Highlight.Identifier
  , _styleReservedHighlight = Highlight.ReservedIdentifier
} where
  varInit = digit <|> letter <|> oneOf "!$%&*/:<=>?~_^-+"
  varSubsequent = varInit <|> oneOf ".+="

tokenParser :: (String -> Maybe r) -> Parser r
tokenParser f = try $ do 
  x <- ident variable
  maybe mzero return $ f x

intParser :: Parser SExpr
intParser = tokenParser (fmap AtomNum . readMaybe) <?> "int"

symParser :: Parser SExpr
symParser = tokenParser symbol <?> "symbol" where
  symbol (x:xs) | not (isDigit x) = Just $ AtomSym (x:xs)
  symbol _ = Nothing

listParser :: Parser SExpr
listParser = List <$> (parens recur <|> brackets recur) where
  recur = many sexprParser

runParser :: Parser a -> String -> ParseResult a
runParser p s = case parseByteString p mempty (UTF8.fromString s) of
  Failure xs -> error (show xs) -- Left $ show xs
  Success a -> Right a

runParserOrDie :: Parser a -> String -> a
runParserOrDie p s = either error id (runParser p s)

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
