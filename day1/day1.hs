
import Data.Bifunctor (Bifunctor(bimap))
import Data.List (sort)
import Data.IntMap (empty)

partOne :: ([Int], [Int]) -> Int
partOne = sum . map abs . uncurry (zipWith (-)) . bimap sort sort 
partTwo :: ([Int], [Int]) -> Int
partTwo input = foldl (\acc x -> acc + x * count x (snd input)) 0 (fst input)
    where count x = length . filter (x ==)

parse :: String -> ([Int], [Int])
parse = unzip . map (bimap read read . \x -> (take 5 x, drop 8 x)) . lines 
main = do
    contents <- readFile "day1/input.txt"
    let con = parse contents
    print $ partOne con
    print $ partTwo con