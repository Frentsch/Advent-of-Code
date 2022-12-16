import System.IO
import Data.List
import Data.Maybe

main = do
    input <- readFile("input.txt")
    let paths = map words $ lines input
    let rocks = drawPaths paths [[False | y<-[0..175]] | x<-[0..360]]
    --print (map (take 10) (take 12 (drop 43 rocks)))
    print (sand 0 rocks)

readInt:: String -> Int
readInt = read

sand count m
    |m!!0!!0 = -1
    |m!!180!!0 = count
    |otherwise = sand (count+1) newm 
    where
        newm = dropsand 180 0 m

dropsand x y m 
    | x<1 ||x+1>= length m = set m 0 0 True 
    | y+1== 172 = set m x y True
    | not (m!!x!!(y+1)) = dropsand x (y+1) m 
    | not (m!!(x-1)!!(y+1)) = dropsand (x-1) (y+1) m 
    | not (m!!(x+1)!!(y+1)) = dropsand (x+1) (y+1) m
    |otherwise = set m x y True 


drawPaths [] m = m
drawPaths (p:paths) m = drawPaths paths (drawline (drop 3 p) m (readInt (p!!0) -320) (read (p!!1)))

drawline [] m old_x old_y= m
drawline (x:y:ls) m old_x old_y
    |old_x==newx &&old_y==newy = drawline (drop 1 ls) (set m old_x old_y True) newx newy
    | old_x==newx =     drawline (x:y:ls) (set m (old_x) (old_y) True) old_x (if diry then old_y-1 else old_y+1)
    | old_y==newy =     drawline (x:y:ls) (set m (old_x) (old_y) True) (if dirx then old_x-1 else old_x+1) old_y
    where 
        newx = readInt x -320
        newy = readInt y 
        dirx = old_x-newx >= 0 
        diry = old_y -newy >= 0


set m x y value =(take x m)++(((take y (m!!x)) ++ (value:(drop (y+1) (m!!x)))):(drop (x+1) m))