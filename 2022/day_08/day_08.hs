import System.IO
import Data.Char

main = do
    contents <- readFile "input.txt"
    let tree = map (map digitToInt)  (lines contents)
    print (sumover (solve tree 0))
    print (maximum( multi (solve tree 1)))

solve t c = [walk d r (x+r) (y+d) (length t) t ((t!!y)!!x) c| x<-[0..((length t)-1)], y<-[0..((length t)-1)], (d,r)<-[(-1,0),(0,-1),(1,0),(0,1)]]

multi [] = []
multi ls = (foldr (*) 1 (take 4 ls)):(multi (drop 4 ls))

sumover [] = 0
sumover ls = (sumover (drop 4 ls)) +if (sum (take 4 ls) >= 1) then 1 else 0

walk d r x y dim t h c
    | x>=dim || y>=dim || x<0 || y<0 = if(c==0) then 1 else (c-1)
    | (t!!y)!!x>=h = c
    | otherwise = walk d r (x+r) (y+d) dim t h (if(c==0)then 0 else (c+1))

