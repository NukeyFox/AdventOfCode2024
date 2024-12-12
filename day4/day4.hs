data Op = Hor | Ver | DiagL | DiagR deriving (Show, Eq)

getWordfromGrid :: Op -> [[Char]] -> Int -> Int -> Int -> [Char]
getWordfromGrid op grid x y len = case op of
    Hor -> [grid !! y !! (x+i) | i <- [0..len-1]]
    Ver -> [grid !! (y+i) !! x | i <- [0..len-1]]
    DiagL -> [grid !! (y+len-i) !! (x+i) | i <- [0..len-1]]
    DiagR -> [grid !! (y+i) !! (x+i) | i <- [0..len-1]]
isXMAS :: [Char] -> Bool
isXMAS xs = xs == "XMAS" || xs == "SAMX" 

countGrid :: [[Char]] -> Op -> Int 
countGrid grid op = length $ filter isXMAS [getWordfromGrid op grid x y 4 | x <- [0..length (head grid) - 4], y <- [0..length grid - 4]]

partOne :: [[Char]] -> Int
partOne grid = sum [countGrid grid op | op <- [Hor, Ver, DiagL, DiagR]]

parse :: String -> [[Char]]
parse = lines
main = do
    raw <- readFile "day4/test_input.txt"
    let input = parse raw
    print $ partOne input
    --print $ partTwo input