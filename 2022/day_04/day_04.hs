import System.IO
import Control.Monad
main = do
    contents <- readFile "input.txt"
    let line = lines contents
    let input = (map (map readInt) (map words line))
    let part1 = sub 0 input
    let part2 = sub1 0 input
    print (part1)
    print (part2)

readInt :: String -> Int
readInt = read

sub sum list
    | list == [] = sum
    | otherwise = sub (sum + (if((nl!!0 -nl!!2)*(nl!!1 -nl!!3)<=0) then 1  else 0)) (tail list)
    where nl = head list

sub1 sum list
    | list == [] = sum
    | otherwise = sub1 (sum +if((nl!!0<=nl!!2 && nl!!2 <=nl!!1) || (nl!!0 <=nl!!3 && nl!!3<=nl!!1)||((nl!!0 -nl!!2)*(nl!!1 -nl!!3)<=0)) then 1 else 0) (tail list)
    where nl = head list

--part1 :: [[Int]] -> Int
--part1 = maximum . map sum 

--part2 :: [[Int]] -> Int
--part2 = sum  . take 3 .reverse . sort . map sum