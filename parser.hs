module Main where

import Control.Monad
import Numeric (readHex, readOct, readFloat)
import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)

main :: IO ()
main = do
  args <- getArgs
  putStrLn (readExpr (head args))

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=?>@^_~"

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
  Left err -> "no match: " ++ show err
  Right val -> "found value"

spaces :: Parser ()
spaces = skipMany1 space

escapedChars :: Parser String
escapedChars = do
  char '\\' -- a backslash
  x <- oneOf "\\\"nrt" -- either backslash or doublequote
  case x of
    '\\' -> do return [x]
    '"' -> do return [x]
    'n' -> do return "\n"
    'r' -> do return "r"
    't' -> do return "\t"

data LispVal
  = Atom String
  | List [LispVal]
  | DottedList [LispVal] LispVal
  | Number Integer
  | String String
  | Bool Bool
  | Float Double

parseExpr :: Parser LispVal
parseExpr =
  parseAtom
    <|> parseString
    <|> parseNumber
    <|> parseBool

parseAtom :: Parser LispVal
parseAtom = do
  first <- letter <|> symbol
  rest <- many (letter <|> digit <|> symbol)
  let atom = first : rest
  return $ case atom of
    "#t" -> Bool True
    "#f" -> Bool False
    _ -> Atom atom

parseString :: Parser LispVal
parseString = do
  char '"'
  x <- many $ many1 (noneOf "\\\"") <|> escapedChars
  char '"'
  return $ String (concat x)

parseNumber :: Parser LispVal
parseNumber = do parseDigInt1 <|> parseDigInt2 <|> parseHex <|> parseOct <|> parseBin

parseDigInt1 :: Parser LispVal
parseDigInt1 = do
  x <- many1 digit
  (return . Number . read) x

parseDigInt2 :: Parser LispVal
parseDigInt2 = do
  try $ string "#d"
  x <- many1 digit
  (return . Number . read) x

parseFloat :: Parser LispVal
parseFloat = do
    x <- many1 digit
    char '.'
    y <- many1 digit
    return $ Float (read (x ++ "." ++ y)) -- made some changes. Bug may appear.

parseHex :: Parser LispVal
parseHex = do
  try $ string "#x"
  x <- many1 hexDigit
  return $ Number (hex2dig x)

parseOct :: Parser LispVal
parseOct = do
  try $ string "#o"
  x <- many1 octDigit
  return $ Number (oct2dig x)

parseBin :: Parser LispVal
parseBin = do
  try $ string "#b"
  x <- many1 (oneOf "10")
  return $ Number (bin2dig x)

oct2dig :: (Eq a, Num a) => String -> a
oct2dig x = fst $ head (readOct x)

hex2dig :: (Eq a, Num a) => String -> a
hex2dig x = fst $ head (readHex x)

bin2dig :: [Char] -> Integer
bin2dig = bin2dig' 0

bin2dig' :: Num t => t -> [Char] -> t
bin2dig' digint "" = digint
bin2dig' digint (x : xs) =
  let old =
        2 * digint
          + ( if x == '0'
                then 0
                else 1
            )
   in bin2dig' old xs

-- parseNumber = liftM (Number . read) $ many1 digit
-- parseNumber = do    x <- many1 digit
--                     (return . Number . read) x
-- parseNumber = many1 digit >>= return . Number . read

parseBool :: Parser LispVal
parseBool = do
  string "#"
  x <- oneOf "tf"
  return $ case x of
    't' -> Bool True
    'f' -> Bool False