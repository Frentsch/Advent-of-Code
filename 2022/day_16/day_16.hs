import System.IO 
import Data.List

data Vertex = V String Int [String] deriving(Show)
main = do
    input <- readFile("input.txt")
    let valves = map words (lines input)
    let graph = createGraph valves [] 
    
    --print (part1 30 graph [] "AA" 0)
    print (part2 26 26 graph [] "AA" "AA" 0)

readInt :: String ->Int 
readInt = read
createGraph [] graph = graph 
createGraph (line:input) graph = createGraph input ((V (line!!0) (readInt (line!!1)) (drop 2 line)):graph)

part2 0 0 _ _ posme posele total = total 
part2 merem elerem graph open posme posele total = if (length (nextoptionsme ++nextoptionsele) ==0) then total else maximum (memoves++elemoves)
    where 
        nextoptionsme = findnext [posme] graph open [] 0 [] [] 
        nextoptionsele = findnext [posele] graph open [] 0 [] [] 
        memoves = (map (\((n,dist),p)-> if(merem-dist-1>=0) then part2 (merem-dist-1) elerem graph (n:open) n posele (total+((merem-dist-1)*p)) else part2 0 elerem graph open n posele total) nextoptionsme)
        elemoves = (map (\((n,dist),p)-> if(elerem-dist-1>=0) then part2 merem (elerem-dist-1) graph (n:open) posme n (total+((elerem-dist-1)*p)) else part2 merem 0 graph open posme n  total) nextoptionsele)


part1 0 _ _ pos total = total 
part1 remaining graph open pos total = if (length nextoptions==0) then total else maximum (map (\((n,dist),p)-> if(remaining-dist-1>=0) then part1 (remaining-dist-1) graph (n:open) n (total+((remaining-dist-1)*p)) else total) nextoptions)
    where 
        nextoptions = findnext [pos] graph open [] 0 [] [] 

findnext [] graph open visited dist nextq found = if(length graph == length visited)then found else findnext nextq graph open visited (dist+1) [] found
findnext (cur:queue) graph open visited dist nextq found 
    | elem cur visited = findnext queue graph open visited dist nextq found 
    | length graph == length visited = found 
    | preassure /= 0 && not (elem cur open) = findnext queue graph open (cur:visited) dist (adj ++ nextq) (((cur,dist),preassure):found) 
    |otherwise = findnext queue graph open (cur:visited) dist (adj ++ nextq) found  
    where 
        V name preassure adj = head (filter (\(V n x y) -> n==cur) graph) 