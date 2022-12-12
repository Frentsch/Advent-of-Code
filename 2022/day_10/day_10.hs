import System.IO
import Data.Char
import Data.List

main = do
    contents <- readFile "input.txt"
    let input =  map words (lines contents)
    print(execute input 0 1 1)
    let array = draw input [] ['.'|x<-[0..39],y<-[0..5]] 0 1
    print( take 40 array)
    print( take 40 (drop 40 array))
    print( take 40 (drop 80 array))
    print( take 40 (drop 120 array))
    print( take 40 (drop 160 array))
    print( take 40 (drop 200 array))

readInt:: String -> Int 
readInt = read

draw [] written m cycle x = m
draw (i:instr) written m cycle x= 
    case i of 
        "noop":[] -> if (abs ((mod cycle 40)-x)<=1)then  draw instr (cycle:written) (set m cycle) (cycle+1) x else draw instr written m (cycle+1) x
        "addx":y:[] -> if (abs ((mod cycle 40)-x)<=1)then draw instr (cycle:written) (set m cycle) (cycle+2) (x+(readInt y)) else (if(x-(mod cycle 40)==2) then draw instr ((cycle+1):written) (set m (cycle+1)) (cycle+2) (x+(readInt y)) else draw instr written m (cycle+2) (x+(readInt y)))

set m cycle=(take cycle m)++ ('#': drop (cycle+1) m )

execute [] total cycle x = total
execute (i:instr) total cycle x= 
    case i of 
        "noop":[] -> if ((cycle-20) `mod` 40==0 )then  execute instr ((cycle*x)+total) (cycle+1) x else execute instr total (cycle+1) x
        "addx":y:[] -> if ((cycle-20) `mod` 40==0 || (cycle-19) `mod` 40==0 )then execute instr (((cycle+(if ((cycle-19) `mod` 40==0) then 1 else 0))*x)+total) (cycle+2) (x+(readInt y)) else execute instr total (cycle+2) (x+(readInt y))
