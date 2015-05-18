{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DefaultSignatures #-}

module L.Parser.SExpr
  (
    ParseResult
  ,	SExpr(..)
  , AsSExpr(..)
  , FromSExpr(..)
  , flatten
  , intParser
  , liftParser
  , list
  , num
  , parseError
  , parseError_
  , runParser
  , runParserOrDie
  , showAsList
  , showSExpr
  , sexprParser
  , sreadWithRest
  , sread
  , sym
  , trim
  ) where

import Control.Applicative
import Control.Monad
import Data.ByteString.UTF8 as UTF8 hiding (lines)
import Data.Char (isDigit, isSpace)
import qualified Data.HashSet as HashSet
import Data.Int
import Data.Semigroup
import L.Util.Utils (mkString)
import Text.Parser.Combinators
import Text.Parser.Token
import qualified Text.Parser.Token.Highlight as Highlight
import Text.Read (readMaybe)
import Text.Trifecta hiding (semi)

data SExpr = AtomSym String | AtomNum Int64 | List [SExpr] deriving (Eq)

sym :: String -> SExpr
sym = AtomSym

num :: Int64 -> SExpr
num = AtomNum

list :: AsSExpr a => [a] -> SExpr
list = List . fmap asSExpr

instance Show SExpr where
  show (AtomSym s) = s
  show (AtomNum i) = show i
  show (List exps) = showAsList $ fmap show exps

showSExpr :: AsSExpr a => a -> String
showSExpr = show . asSExpr

class AsSExpr a where
  asSExpr :: a -> SExpr

--instance AsSExpr String where
--  asSExpr = AtomSym

instance AsSExpr Int64 where
  asSExpr = AtomNum

instance AsSExpr SExpr where
  asSExpr = id

instance AsSExpr a => AsSExpr [a] where 
  asSExpr as = List $ fmap asSExpr as

instance (AsSExpr a, AsSExpr b) => AsSExpr (a,b) where 
  asSExpr (a,b) = List $ [asSExpr a, asSExpr b]

instance (AsSExpr a, AsSExpr b, AsSExpr c) => AsSExpr (a,b,c) where 
  asSExpr (a,b,c) = List $ [asSExpr a, asSExpr b, asSExpr c]

instance (AsSExpr a, AsSExpr b, AsSExpr c, AsSExpr d) => AsSExpr (a,b,c,d) where 
  asSExpr (a,b,c,d) = List $ [asSExpr a, asSExpr b, asSExpr c, asSExpr d]

instance (AsSExpr a, AsSExpr b, AsSExpr c, AsSExpr d, AsSExpr e) => AsSExpr (a,b,c,d,e) where 
  asSExpr (a,b,c,d,e) = List $ [asSExpr a, asSExpr b, asSExpr c, asSExpr d, asSExpr e]

instance (AsSExpr a, AsSExpr b, AsSExpr c, AsSExpr d, AsSExpr e, AsSExpr f) => AsSExpr (a,b,c,d,e,f) where 
  asSExpr (a,b,c,d,e,f) = List $ [asSExpr a, asSExpr b, asSExpr c, asSExpr d, asSExpr e, asSExpr f]

showAsList :: [String] -> String
showAsList as = "(" ++ (mkString " " as) ++ ")"

{-- PARSER --}

type ParseResult a = Either String a

--class (Monad m) => FromSExpr a where
--  fromSExpr :: SExpr -> m a
class FromSExpr a where
  fromSExpr :: SExpr -> Either String a
  fromString :: String -> Either String a
  default fromString :: String -> Either String a
  fromString = fromSExpr . sread

instance FromSExpr a => FromSExpr [a] where
  fromSExpr (List args) = sequence (fmap fromSExpr args)
  fromSExpr bad         = Left $ "bad list" ++ show bad

parseError :: String -> String -> ParseResult a
parseError msg  exp = Left $ concat ["Parse Error: '", msg, "' in: ", show exp]
-- TODO: rename this
parseError_ :: String -> SExpr -> ParseResult a
parseError_ msg expr = parseError msg (show expr)

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
