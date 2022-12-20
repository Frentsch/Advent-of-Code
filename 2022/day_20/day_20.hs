import System.IO 
import Data.List

main = do
    input <- readFile("input.txt")
    let numbers = map (readInt) (lines input)
    let mult = map (* 811589153) numbers
    let ordered = [(x,numbers!!x)|x<-[0..(length numbers -1)]]
    let multordered = [(x,mult!!x)|x<-[0..(length numbers -1)]]
    let mixxed = mix ordered 0
    let mix10 = mx10 multordered 10
    let zeroindex = iofzero mixxed 0
    let zi = iofzero mix10 0
    print (snd (mixxed!!(mod (zeroindex+1000) (length ordered))) + snd (mixxed!!(mod (zeroindex+2000) (length ordered))) + snd (mixxed!!(mod (zeroindex+3000) (length ordered))) ) 
    print (snd (mix10!!(mod (zi+1000) (length ordered))) + snd (mix10!!(mod (zi+2000) (length ordered))) + snd (mix10!!(mod (zi+3000) (length ordered))) ) 

readInt :: String -> Int 
readInt = read 

mx10 res 0 = res 
mx10 res i = mx10 (mix res 0) (i-1)
mix res x 
    |x== length res = res 
    |otherwise = mix (add (remove res index) next newpos) (x+1)
    where 
        index = findindex res x 0
        next = res!!index 
        shift = mod (snd next) (length res -1)
        newpos = mod (if shift+index<=0 || shift+index>=length res-1 then index+shift+(div shift (abs shift)) else index+shift) (length res)


findindex (x:xs) a i 
    |fst x== a = i 
    |otherwise = findindex xs a (i+1)

remove array index = take index array ++ (drop (index+1) array)
add array elem index = take index array ++ (elem:(drop index array))
iofzero (x:xs) index 
    | snd x == 0 = index
    |otherwise = iofzero xs (index+1)