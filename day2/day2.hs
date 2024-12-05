safe pred list = fst $ foldl (\(acc,prev) x -> (pred prev x && acc, x)) (True, head list) (tail list)
check x y = x - y > 0 && x - y < 4
checkSafe x = safe check  x || safe (flip check) x

removeOne [] = []
removeOne xs = [take i xs ++ drop (i+1) xs | i <- [0..length xs - 1]]

partOne :: [[Int]] -> Int
partOne = length . filter checkSafe

partTwo = length . filter (\xs -> checkSafe xs || any checkSafe (removeOne xs))

parse :: String -> [[Int]]
parse = map (map read . words) . lines
main = do
    contents <- readFile "day2/input.txt"
    let input = parse contents
    print $ partOne input
    print $ partTwo input