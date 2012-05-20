module SudokuParser(parseMatrix) where
import Text.ParserCombinators.Parsec
import Data.Char

parseMatrix :: Parser [[Int]]
parseMatrix =  do 
        result <- count 9 parseLine
        _ <- eof
        return result

parseLine :: Parser [Int]
parseLine = do
        result <- count 9 parseDigit
        _ <- eol
        return result

parseDigit :: Parser Int
parseDigit = do
        result <- digit
        return (digitToInt result)

eol :: Parser String
eol =   try (string "\n\r")
    <|> try (string "\r\n")
    <|> string "\n"
    <|> string "\r"
    <?> "end of line"