import System.IO
import Data.Char
import Data.List
import Prelude
main = do
    input <- readFile("input.txt")
    let line = lines input
    let startindex = (0,0)
    print (bfs line [startindex] [startindex] [] 0)

bfs m visited [] nextq depth = if(depth==1) then (length nextq) else bfs m (nextq++visited) nextq [] (depth+1)
bfs  m visited (t:ts) nextq depth 
    |elem t visited || x<0 || y<0 ||x>= (length m) || y>= length (m!!0)= bfs m visited ts nextq depth
    |(m!!x)!!y== 'E' = depth
    |otherwise = bfs m (visited) ts (nextq++ (if valid m t (x+1,y) then [(x+1,y)] else []) ++(if valid m t (x-1,y) then [(x-1,y)] else [])++ (if valid m t (x,y+1) then [(x,y+1)] else [])++(if valid m t (x,y-1) then [(x,y-1)] else [])) depth
    where 
        (x,y) = t

valid m old new = (ord ((m!!(fst old))!!(snd old))) +1>= ord ((m!!(fst new))!!(snd new))