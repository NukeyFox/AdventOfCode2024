import Data.Char (isDigit)
import System.Posix (GroupEntry(groupID))
data Op = Hor | Ver | DiagL | DiagR deriving (Show, Eq)

getWordfromGrid :: Op -> [[Char]] -> Int -> Int -> Int -> [Char]
getWordfromGrid Hor   grid x y len = [(grid !! y) !! (x+i) | i <- [0..len-1]]
getWordfromGrid Ver   grid x y len = [grid !! (y+i) !! x | i <- [0..len-1]]
getWordfromGrid DiagL grid x y len = [grid !! (y+len-i-1) !! (x+i) | i <- [0..len-1]]
getWordfromGrid DiagR grid x y len = [grid !! (y+i) !! (x+i) | i <- [0..len-1]]


isXMAS :: [Char] -> Bool
isXMAS xs = xs == "XMAS" || xs == "SAMX"

isMAS xs = xs == "MAS" || xs == "SAM"

isMASCross :: [[Char]] -> Int -> Int -> Bool
isMASCross grid x y = isMAS (getWordfromGrid DiagL grid x y 3) && isMAS (getWordfromGrid DiagR grid x y 3)

getWords grid Hor len = [getWordfromGrid Hor grid x y len | x <- [0..(length (head grid) - len)], y <- [0..length grid - 1]]
getWords grid Ver len = [getWordfromGrid Ver grid x y len | x <- [0..length (head grid) - 1], y <- [0..length grid - len]]
getWords grid DiagL len = [getWordfromGrid DiagL grid x y len | x <- [0..length (head grid) - len], y <- [0..length grid - len]]
getWords grid DiagR len = [getWordfromGrid DiagR grid x y len | x <- [0..length (head grid) - len], y <- [0..length grid - len]]
countXMASinGrid :: [[Char]] -> Op -> Int
countXMASinGrid grid op = length $ filter isXMAS $ getWords grid op 4

partOne :: [[Char]] -> Int
partOne grid = sum [countXMASinGrid grid op | op <- [Hor, Ver, DiagL, DiagR]]
partTwo grid =length $ filter (uncurry (isMASCross grid)) [(x, y) | x <- [0..length (head grid) - 3], y <- [0..length grid - 3]]


parse :: String -> [[Char]]
parse = lines
main = do
    raw <- readFile "day4/input.txt"
    let input = parse raw
    print $ partOne input
    print $ partTwo input