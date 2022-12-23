import System.IO 

main = do 
    input <- readFile("input.txt")
    let i = lines input 
    let map = init i
    let path = words (tail map)
    let startpos = findi (map!!0) 0

walk map (x,y) face steps 
    |steps = 0 = (x,y)
    | face == 0 = if(x==length (map!!y)-1 || map!!y!!(x+1)==' ') then wrap else if( map!!y!!(x+1)=='#') then (x,y) else  walk map (x+1,y) face (steps-1) 
    | face == 1 = if(y>=length map -1 || x>=length (map!!(y+1)) || map!!(y+1)!!x ==' ')then wrap else if map!!(y+1)!!x=='#' then (x,y) else  walk map (x,y+1) face (steps-1) 
    | face == 2 = if(x==0 || map!!y!!(x-1)==' ') then wrap else if (map!!y!!(x-1)=='#') then (x,y) else  walk map (x-1,y) face (steps-1) 
    | face == 3 = if(y==0 || x>= length (map!!(y-1)) || map!!(y-1)!!x==' ') then warp else if map!!(y-1)!!x=='#' then (x,y) else walk map (x,y-1) face (steps-1) 

wrap map (x,y) face
    |face==0 = findmap False (map!!y) 0
    |face==1
    |face==2 = (length (map!!y)) -1

findvertical map x reverse i 
    |map!!i!!x/=' ' = i 
    |

findmap line i 
    |line!!i/=' ' = i 
    |otherwise = findmap line ()i+1)

findi line i 
    |line!!i == '.' = i 
    |otherwise = findi line (i+1)