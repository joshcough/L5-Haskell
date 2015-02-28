module L.Read 
  (
    ParseResult
   ,flatten
   ,liftParser
   ,runParser
   ,runParserOrDie
   ,intParser
   ,sexprParser
   ,sreadWithRest
   ,sread
   ,trim
   ,module L.SExpr
  ) where

import Control.Applicative
import Control.Monad
import Data.ByteString.UTF8 as UTF8 hiding (lines)
import Data.Char (isDigit, isSpace)
import qualified Data.HashSet as HashSet
import Data.Semigroup
import L.SExpr
import Text.Parser.Combinators
import Text.Parser.Token
import qualified Text.Parser.Token.Highlight as Highlight
import Text.Read (readMaybe)
import Text.Trifecta hiding (semi)

type ParseResult a = Either String a

trim :: String -> String
trim = f . f where f = reverse . dropWhile isSpace

sread :: String -> SExpr
sread = runParserOrDie sexprParser . preprocess

sreadWithRest :: String -> (SExpr, String)
sreadWithRest = runParserOrDie ((,) <$> sexprParser <*> many anyChar) . preprocess

preprocess :: String -> String
preprocess = trim . concat . map f . lines where
  f = (++ " ") . trim . removeComments
  removeComments = takeWhile $ not . (==';')

flatten :: SExpr -> [SExpr]
flatten (List ss) = ss >>= flatten
flatten s         = [s]

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
symParser = tokenParser sym <?> "symbol" where
  sym (x:xs) | not (isDigit x) = Just $ AtomSym (x:xs)
  sym _ = Nothing

listParser :: Parser SExpr
listParser = List <$> (parens recur <|> brackets recur) where
  recur = many sexprParser

runParser :: Parser a -> String -> ParseResult a
runParser p s = case parseByteString p mempty (UTF8.fromString s) of
  Failure xs -> Left $ show xs
  Success a  -> Right a

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
