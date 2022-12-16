import System.IO
import Data.Char
import Data.Maybe
import Data.List
import Prelude
main = do
    input <- readFile("input.txt")
    let line = lines input
    let startindex = (20,139)
    print (bfs line [] [startindex] [] 0)

bfs m visited [] nextq depth =  bfs m visited nextq [] (depth+1)
bfs m visited (t:ts) nextq depth 
    |elem t visited= bfs m visited ts nextq depth
    |(m!!x)!!y== 'c' = depth 
    |otherwise = bfs m (t:visited) ts (nextq++ (if valid m visited t (x+1,y) then [(x+1,y)] else []) ++(if valid m visited t (x-1,y) then [(x-1,y)] else [])++ (if valid m visited t (x,y+1) then [(x,y+1)] else [])++(if valid m visited t (x,y-1) then [(x,y-1)] else [])) depth
    where 
        (x,y) = t

valid m visited old new 
    |elem new visited || nx<0||ny<0||nx>=length m || ny>= length(m!!0) = False
    |otherwise = (ord oldc) <= (ord newc) +1
    where
        (nx,ny)=new
        (ox,oy)=old
        oldc = (m!!ox)!!oy
        newc = (m!!nx)!!ny