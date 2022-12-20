import System.IO 

main = do
    input <- readFile("input.txt")
    let heights = [False | y<-[0..6]]:[[True|x<-[0..6]]|y<-[0..4000]]
    --print $ length input
    print (part1 1 0 input 4 2 heights) 

part1 rock w winds y x h 
    |rock==2023 =eval h 0
    |modrock == 1 = if h!!ny!!nx &&h!!ny!!(nx+1)&&h!!ny!!(nx+2)&&h!!ny!!(nx+3) then part1 rock (nw+1) winds ny nx h else part1 (rock+1) (nw+1) winds newy 2 newh
    |modrock == 2 = if h!!y!!nx&&h!!ny!!(nx+1)&&h!!y!!(nx+2) then part1 rock (nw+1) winds ny nx h else part1 (rock+1) (nw+1) winds newy 2 newh 
    |modrock == 3 = if h!!ny!!nx&&h!!ny!!(nx+1)&&h!!ny!!(nx+2) then part1 rock (nw+1) winds ny nx h else part1 (rock+1) (nw+1) winds newy 2 newh
    |modrock == 4 = if h!!ny!!nx then part1 rock (nw+1) winds ny nx h else part1 (rock+1) (nw+1) winds newy 2 newh 
    |modrock == 0 = if h!!ny!!nx &&h!!ny!!(nx+1) then part1 rock (nw+1) winds ny nx h else part1 (rock+1) (nw+1) winds newy 2 newh
        where
            nw = mod w 10091
            nx = if(winds!!nw=='<' && x>0) then jet modrock '<' y x h  else (if winds!!nw=='>' && x<6 then jet modrock '>' y x h else x)
            ny=y-1
            modrock = mod rock 5
            newh = setrock modrock y nx h
            newy = eval newh 0 +4
    
part2 rock w winds y x h mem
    |modrock == 1 = if h!!ny!!nx &&h!!ny!!(nx+1)&&h!!ny!!(nx+2)&&h!!ny!!(nx+3) then part2 rock (nw+1) winds ny nx h else (if length filtered ==1 then ((w,modrock),(h!!y)):filtered else part2 (rock+1) (nw+1) winds newy 2 newh)
    |modrock == 2 = if h!!y!!nx&&h!!ny!!(nx+1)&&h!!y!!(nx+2) then part2 rock (nw+1) winds ny nx h else part2 (rock+1) (nw+1) winds newy 2 newh 
    |modrock == 3 = if h!!ny!!nx&&h!!ny!!(nx+1)&&h!!ny!!(nx+2) then part2 rock (nw+1) winds ny nx h else part2 (rock+1) (nw+1) winds newy 2 newh
    |modrock == 4 = if h!!ny!!nx then part2 rock (nw+1) winds ny nx h else part2 (rock+1) (nw+1) winds newy 2 newh 
    |modrock == 0 = if h!!ny!!nx &&h!!ny!!(nx+1) then part2 rock (nw+1) winds ny nx h else part2 (rock+1) (nw+1) winds newy 2 newh
        where
            nw = mod w 10091
            nx = if(winds!!nw=='<' && x>0) then jet modrock '<' y x h  else (if winds!!nw=='>' && x<6 then jet modrock '>' y x h else x)
            ny=y-1
            modrock = mod rock 5
            newh = setrock modrock y nx h
            newy = eval newh 0 +4
            filtered = filter (\((ji, mr),(tl,lvl))->ji==w && mr ==modrock && tl == h!!y) mem

setrock rock y x h 
    |rock == 1 = set y x (set y (x+1) (set y (x+2) (set y (x+3) h)))
    |rock == 2 = set (y+1) x (set y (x+1) (set (y+1) (x+1) (set (y+2) (x+1) (set (y+1) (x+2) h))))
    |rock == 3 = set y x (set y (x+1) (set y (x+2) (set (y+1) (x+2) (set (y+2) (x+2) h))))
    |rock == 4 = set y x (set (y+1) x (set (y+2) x (set (y+3) x h)))
    |rock == 0 = set y x (set (y+1) x (set y (x+1) (set (y+1) (x+1) h)))

set y x  h = take y h ++ (take x (h!!y) ++ False:(drop (x+1) (h!!y))):(drop (y+1) h)

jet rock w y x h 
    |rock == 1 = if w=='<' && (h!!y!!(x-1)) then x-1 else (if w=='>' &&x<3 && (h!!y!!(x+4))then x+1 else x)
    |rock == 2 = if w=='<' && (h!!(y+1)!!(x-1)) && (h!!y!!x) then x-1 else if w=='>' && x<4 && (h!!(y+1)!!(x+3))&& (h!!y!!(x+2)) then x+1 else x
    |rock == 3 = if w=='<' && (h!!y!!(x-1)) then x-1 else if w=='>' && x<4 && h!!y!!(x+3) &&h!!(y+1)!!(x+3) && h!!(y+2)!!(x+3) then x+1 else x
    |rock == 4 = if w=='<' && h!!y!!(x-1) && h!!(y+1)!!(x-1)&&h!!(y+2)!!(x-1)&&h!!(y+3)!!(x-1) then x-1 else if w=='>'&&h!!y!!(x+1)&&h!!(y+1)!!(x+1)&&h!!(y+2)!!(x+1)&&h!!(y+3)!!(x+1) then x+1 else x
    |rock == 0 = if w=='<' && h!!y!!(x-1) && h!!(y+1)!!(x-1) then x-1 else if w=='>' && x<5 && h!!y!!(x+2) && h!!(y+1)!!(x+2) then x+1 else x


eval heights i 
    | and (heights!!i) =  i-1
    |otherwise = eval heights (i+1)