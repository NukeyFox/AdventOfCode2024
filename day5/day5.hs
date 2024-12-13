import Text.Parsec.String (Parser)
import Text.Parsec (many1, char, newline, digit, parse, sepBy1)
import Control.Applicative
import Data.List (elemIndex)
import Data.Maybe (mapMaybe, fromMaybe)


parseRule :: Parser (Int, Int)
parseRule = do
    l <- many1 digit
    _ <- char '|'
    r <- many1 digit
    _ <- newline
    return (read l,read r)

parseUpdate = do
    _ <- newline
    num <- sepBy1 (many1 digit) (char ',')
    return $ map read num

parseInput :: String -> ([(Int, Int)], [[Int]])
parseInput input = case parse parser "" input of
    Left err -> ([],[])
    Right res -> res
    where parser = do
            rules <- many1 parseRule
            updates <- many1 parseUpdate
            return (rules, updates)

obeysRule :: (Int, Int) -> [Int] -> Maybe Bool
obeysRule (l,r) update = do
    li <- elemIndex l update
    ri <- elemIndex r update
    return (li < ri)

obeysAllRules rules update = and (mapMaybe (`obeysRule` update) rules)

getMiddleEement :: [Int] -> Int
getMiddleEement update = update !! (length update `div` 2)

swapElements :: Int -> Int -> [Int] -> [Int]
swapElements i j = map (\x -> if x == i then j else if x == j then i else x)

fixUpdate :: (Int, Int) -> [Int] -> [Int]
fixUpdate (l,r) update = case obeysRule (l,r) update of
    Just True -> update
    Just False -> swapElements l r update
    Nothing -> update

fixAllUpdates :: [(Int, Int)] -> [Int] -> [Int]
fixAllUpdates rules update = foldl (flip fixUpdate) update rules

partOne (rules, updates) =  sum $ map getMiddleEement $ filter (obeysAllRules rules) updates
partTwo (rules, updates) =  sum
        $ map (getMiddleEement . (!! 4) . iterate (fixAllUpdates rules)) 
        (filter (not . obeysAllRules rules) updates)
main = do
    raw <- readFile "day5/input.txt"
    let input = parseInput raw
    --print $ partOne input
    print $ partTwo input