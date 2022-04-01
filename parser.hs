module Main where
import Control.Monad
import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)

main :: IO ()
main = do   args <- getArgs
            putStrLn (readExpr (args !! 0))

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=?>@^_~#"

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
    Left err -> "no match: " ++ show err
    Right val -> "found value"

spaces :: Parser ()
spaces = skipMany1 space

escapedChars :: Parser String
escapedChars =  do  char '\\' -- a backslash
                    x <- oneOf "\\\"nrt" -- either backslash or doublequote
                    case x of
                        '\\' -> do return [x]
                        '"' -> do return [x]
                        'n' -> do return "\n"
                        'r' -> do return "r"
                        't' -> do return "\t"


data LispVal =  Atom String
             |  List [LispVal]
             |  DottedList [LispVal] LispVal
             |  Number Integer
             |  String String
             |  Bool Bool

parseString :: Parser LispVal
parseString =   do  char '"'
                    x <- many $ many1 (noneOf "\\\"") <|> escapedChars
                    char '"'
                    return $ String (concat x)

parseAtom :: Parser LispVal
parseAtom = do  first <- letter <|> symbol
                rest <- many (letter <|> digit <|> symbol)
                let atom = [first] ++ rest
                return $ case atom of
                            "#t" -> Bool True
                            "#f" -> Bool False
                            otherwise -> Atom atom

parseNumber :: Parser LispVal
-- parseNumber = liftM (Number . read) $ many1 digit
-- parseNumber = do    x <- many1 digit
--                     (return . Number . read) x
parseNumber = many1 digit >>= return . Number . read

parseExpr :: Parser LispVal
parseExpr = parseAtom
        <|> parseString
        <|> parseNumber

