import Text.Parsec.String (Parser)
import Text.Parsec (string, manyTill, many1, digit, char, parse, try, anyChar, lookAhead, eof, skipMany, noneOf)
import Control.Applicative

parseMul :: Parser Int
parseMul = do
    _ <- string "mul"
    _ <- char '('
    x <- many1 digit
    _ <- char ','
    y <- many1 digit
    _ <- char ')'
    return (read x * read y)

parseDoDont :: Parser String
parseDoDont = do
    _ <- try (string "do()")
    manyTill anyChar (lookAhead (try (string "don't()") <|> (eof >> return "EOF")))

parseAll :: Parser a -> Parser [a]
parseAll p = concat <$> many (try (pure <$> p) <|> (noneOf "" >> return []))

parseAllMul :: Parser [Int]
parseAllMul = parseAll parseMul

parseAllDoDonts :: Parser [String]
parseAllDoDonts = parseAll parseDoDont
partOne input = case parse parseAllMul "" input of
    Left err -> -1
    Right val -> sum val

partTwo input = case parse parseAllDoDonts "" ("do()"++input++"don't()") of
    Left err -> 0
    Right val -> sum $ map partOne val

main = do
    input <- readFile "day3/input.txt"
    print $ partOne input
    print $ partTwo input
