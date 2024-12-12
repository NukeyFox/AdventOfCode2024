import Data.Char (digitToInt, isDigit)

data Inst = Mul | LNum | RNum Int | Comma | LBkt | RBkt Int Int 
findNum :: String -> (String, String)
findNum = findNumHelper ""
    where findNumHelper num "" = (num, "")
          findNumHelper num (x:xs) = if isDigit x then findNumHelper (num ++ [x]) xs else (num, x:xs)

partOneHelper _ acc "" = acc 
partOneHelper Mul acc ('m':'u':'l':xs) = partOneHelper LBkt acc xs 
partOneHelper Mul acc (x:xs) = partOneHelper Mul acc xs 
partOneHelper LBkt acc ('(':xs) = partOneHelper LNum acc xs 
partOneHelper LBkt acc xs = partOneHelper Mul acc xs
partOneHelper LNum acc xs = if num /= "" then partOneHelper (RNum $ read num) acc substr else partOneHelper Mul acc xs 
    where (num, substr) = findNum xs
partOneHelper (RNum n) acc xs = if num /= "" then partOneHelper (RBkt n (read num)) acc substr else partOneHelper Mul acc xs  
    where (num, substr) = findNum xs 
partOneHelper (RBkt n m) acc (')':xs) = partOneHelper Mul (acc + n * m) xs 
partOneHelper (RBkt _ _) acc xs = partOneHelper Mul acc xs 

findMulMatches = match

partOne = partOneHelper Mul 0 

main = do
    input <- readFile "day3/input.txt"
    print $ partOne input