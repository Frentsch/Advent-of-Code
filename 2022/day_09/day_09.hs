import System.IO
import Data.Char
import Data.List

main = do
    contents <- readFile "input.txt"
    let input =  map words (lines contents)
    let dir = map (\(x:y:xs) -> (x, readInt y)) input
    let head_path =visit [] dir 0 0 0 0
    let filtered_path = filterpath [] (reverse head_path)
    print(length filtered_path)
    let tail_path = re 8 (filtered_path)
    print (length (nub head_path))
    print (length (nub tail_path))


readInt:: String -> Int 
readInt = read 

re num old_path
    |num==0 = old_path
    |otherwise = re (num-1) (walk [] old_path 0 0)

walk path [] xt yt = filterpath [] ( reverse ((xt,yt):path) )
walk path ((a,b):rest) xt yt =walk ((nx,ny):path) rest nx ny
    where 
        (nx,ny) = move a b xt yt

visit visited []  xh yh xt yt= (xt,yt):visited
visit visited ((a,b):rest) xh yh xt yt
    | b <=  0 = visit ((nx,ny):visited) rest xh yh nx ny
    | a =="U" = visit ((nx,ny):visited) ((a, b-1):rest) xh (yh+1) nx ny
    | a =="D" = visit ((nx,ny):visited) ((a, b-1):rest) xh (yh-1) nx ny
    | a =="R" = visit ((nx,ny):visited) ((a, b-1):rest) (xh+1) (yh) nx ny
    | a =="L" = visit ((nx,ny):visited) ((a, b-1):rest) (xh-1) (yh) nx ny
    where 
        (nx,ny) = move xh yh xt yt

move a b c d
    | abs (a-c)>=2 && abs (b-d)>=2 =  (if(a-c>=0) then (c+1) else (c-1), if(b-d>=0) then (d+1) else (d-1))
    | abs (a-c)>=2 = (if(a-c>=0) then (c+1) else (c-1), b)
    | abs (b-d)>=2 = (a, if(b-d>=0) then (d+1) else (d-1))
    |otherwise = (c,d)
    
filterpath  path [] = reverse path
filterpath  path (dir:rest) = filterpath (dir:path) (dropWhile (==dir) rest)
