import System.IO 

main = do 
    input <- readFile("input.txt")
    let cords = map (map readInt) (map words (lines input))
    let adj = map (map (+1)) cords
    let grid = [[[0|z<-[0..26]]|y<-[0..26]]|x<-[0..26]]
    let painted = setall adj grid
    let sol1 = (part1 adj grid 0)
    let sol2 = (part2 painted 0 (1,1,1))
    print sol1
    print sol2 
    print (sol1 + sol2)

    --print ((part1 adj grid 0) + (part2 (setall adj grid) 0 (1,1,1)))
    
    

part1 [] grid sum = sum
part1 (v:lava) grid sum = part1 lava (set (x,y,z) grid) (sum+6-(2*surround))
    where
        x:y:z:[] = v
        surround = grid!!x!!y!!(z+1) + grid!!x!!y!!(z-1) + grid!!x!!(y+1)!!z + grid!!x!!(y- 1)!!z + grid!!(x+1)!!y!!z  + grid!!(x-1)!!y!!z

setall [] grid = grid 
setall (l:lava) grid = setall lava (set (x,y,z) grid)
    where 
        x:y:z:[] =l 


part2 grid sum (x,y,z) 
    | x>=23 && y>=23 && z>=23 = sum
    | z >= 24 = part2 grid sum (x,y+1,1)
    | y >= 24 = part2 grid sum (x+1, 1,1)
    | otherwise = if(center) then part2 (set (x,y,z) grid) (sum+6-(2*surround)) (x,y,z+1) else part2 grid sum (x,y,z+1)
    where 
        surround = grid!!x!!y!!(z+1) + grid!!x!!y!!(z-1) + grid!!x!!(y+1)!!z + grid!!x!!(y- 1)!!z + grid!!(x+1)!!y!!z  + grid!!(x-1)!!y!!z
        center =  grid!!x!!y!!z==0 && bfs grid [(x,y,z)] [] []

bfs grid [] [] visited = True 
bfs grid [] nextq visited = bfs grid nextq [] visited
bfs grid ((x,y,z):q) nextq visited
    | x>24 || y>24 ||z >24 || x<=0 || y<=0 || z<=0 = False
    | grid!!x!!y!!z==1 =  bfs grid q nextq visited
    | otherwise =bfs grid q (nextq++filtered) (filtered++visited)
    where
        filtered = filter (\(a,b,c)-> not (elem (a,b,c) visited) && grid!!a!!b!!c==0) [(x,y,z+1),(x,y,z-1),(x,y+1,z),(x,y-1,z),(x+1,y,z),(x-1,y,z)]

readInt :: String -> Int 
readInt = read 

set (x,y,z) grid = take x grid ++ ((take y (grid!!x) ++ ((take z (grid!!x!!y)) ++ 1:(drop (z+1) (grid!!x!!y))):(drop (y+1) (grid!!x))):(drop (x+1) (grid)))