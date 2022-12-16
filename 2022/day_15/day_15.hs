import System.IO 
import Data.List
main = do
    input <- readFile("input.txt")
    let line = map words $ lines input 
    let sensors = map (map readInt) line
    print (2727057 * 4000000 + 2916597)
    --print (part2 sensors sensors)

readInt :: String -> Int 
readInt = read 

solve i n sensors = if(length res >1 ||i>=n) then res else solve (i+1) n sensors
    where 
        res = meld (part1 sensors [] i)


meld old 
    |length old == length new = new
    |otherwise = meld new 
    where 
        res = combine old old
        new = nub (contained res [] res)

contained [] new list = new 
contained ((a,b):old) new list = if ( or (map (\(c,d)-> c<=a &&b<=d &&(a/=c || b/=d)) list)) then contained old new list else contained old ((a,b):new) list 

combine [] update = update
combine (entry:old) update =combine old (map (\(a,b) -> if (start<=a && a <=end) then (start, b) else (if start <= b && b <=end then (a,end) else (a, b))) update) 
    where 
        (start,end) = entry

part1 [] positions level = positions 
part1 (sensor:sensors) positions level = if(remaining>=0) then part1 sensors ((x-remaining,x+remaining):positions)level else part1 sensors positions level
    where
        x:y:bx:by:[] = sensor 
        distance = abs (bx-x) + abs (by-y)
        remaining = distance- abs (level - y)


part2 [] list = [(-2,-2)]
part2 (sensor:sensors) list = if(length filtered >=1) then filtered else part2 sensors list
    where 
        x:y:bx:by:[] = sensor
        distance = abs (bx-x) + abs (by-y)
        res = [walk (x+distance +1) y (\a->a- 1) (\a->a- 1) x (y-distance-1) list, walk (x+distance +1) y (\a->a- 1) (\a->a+ 1) x (y+distance+1) list,walk x (y-distance-1) (\a->a- 1) (\a->a + 1) (x-distance-1) y list,walk x (y+distance+1) (\a->a- 1) (\a->a- 1) (x-distance-1) y list]
        filtered = filter (/=(-1,-1)) res

walk x y dx dy destx desty sensors 
    | x ==destx &&y==desty = res
    |res==(-1,-1) = walk (dx x) (dy y) dx dy destx desty sensors 
    |otherwise = res
        where 
            res = test x y sensors

test tx ty [] = (tx,ty)
test tx ty (sensor:sensors) 
    | abs (tx-x) + abs (ty-y) <= distance || tx<0 || ty<0 || tx>4000000 || ty>4000000 = (-1,-1)
    |otherwise = test tx ty sensors
    where 
        x:y:bx:by:[] = sensor 
        distance = abs (bx-x) + abs (by-y)