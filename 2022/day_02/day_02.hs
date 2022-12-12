import System.IO
import Control.Monad
import Data.List

main = do
    contents <- readFile "input.txt"
    let line = lines contents
    print (part1 0 line)
    print (part2 0 line)

readInt :: String -> Int
readInt = read


sub total current list
    | list == [] = total
    | head list /= [] = sub total ((readInt (head list)):current) (tail list)
    | otherwise = sub (current:total) [] (tail list)


part1 sum [] = sum
part1 sum (a:list) = case a of
    "A X" -> part1 (sum+4) list
    "A Y" -> part1 (sum+8) list
    "A Z" -> part1 (sum+3) list
    "B X" -> part1 (sum+1) list
    "B Y" ->part1 (sum+5) list
    "B Z" ->part1 (sum+9) list
    "C X"->part1 (sum+7) list
    "C Y" ->part1 (sum+2) list
    "C Z" ->part1 (sum+6) list

part2 sum [] = sum
part2 sum (a:list) = case a of
    "A X" -> part2 (sum+3) list
    "A Y" -> part2 (sum+4) list
    "A Z" -> part2 (sum+8) list
    "B X" -> part2 (sum+1) list
    "B Y" ->part2 (sum+5) list
    "B Z" ->part2 (sum+9) list
    "C X"->part2 (sum+2) list
    "C Y" ->part2 (sum+6) list
    "C Z" ->part2 (sum+7) list

