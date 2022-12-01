import System.IO
import Control.Monad
import Data.List
main = do
    contents <- readFile "input.txt"
    let line = lines contents
    let list = sub [[]] [] line
    print (part1 list)
    print (part2 list)

readInt :: String -> Int
readInt = read

sub :: [[Int]] ->[Int] ->[String] -> [[Int]]
sub total current list
    | list == [] = total
    | head list /= [] = sub total ((readInt (head list)):current) (tail list)
    | otherwise = sub (current:total) [] (tail list)

part1 :: [[Int]] -> Int
part1 = maximum . map sum 

part2 :: [[Int]] -> Int
part2 = sum  . take 3 .reverse . sort . map sum