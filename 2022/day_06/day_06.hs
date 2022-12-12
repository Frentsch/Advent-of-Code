import System.IO
import Data.List
main = do
    contents <- readFile "input.txt"
    let line = lines contents
    print (part (head line) 4 4)
    print (part (head line) 14 14)

readInt :: String -> Int
readInt = read

part line i x
 |line==[] = i 
 |otherwise = if (length(nub (take x line))==x) then i else part (drop 1 line) (i+1) x