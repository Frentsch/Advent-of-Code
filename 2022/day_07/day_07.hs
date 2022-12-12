import System.IO
import Data.List
import Data.Typeable

data Obj = Dir String [Obj] | File Int String deriving(Show)
data IntArray = Num Int | Arr Int [IntArray] deriving(Show)
main = do
    contents <- readFile "input.txt"
    let line = tail (lines contents)
    let commands = map words line
    --print (createTree commands (Dir "/" []))
    let array = (sumarray (createTree commands (Dir "/" [])))
    print (computesize array)
    print (required array)
    print (deletable array array)

readInt :: String -> Int
readInt = read

required (Arr size sub) = (30000000-(70000000-size))
deletable array (Arr size sub) = findmin (30000000-(70000000-size)) 70000000 array 
findmin _ m (Num size) =  m
findmin req m (Arr size sub) = if(size>=req )then minimum (size:(map (findmin req m) sub)) else minimum (map (findmin req m) sub)

--foldDir d f dir = case dir of 
sumarray (File size name) = Num size
sumarray (Dir name files) = Arr (sumup (map (sumarray) files)) (map (sumarray) files)

sumup [] = 0
sumup ((Num size):rest) = size + (sumup rest)
sumup ((Arr size sub):rest) = size + (sumup rest)  


computesize (Num size)=0
computesize (Arr size array) = (sum (map computesize array)) + if(size<=100000)then size else 0

removeallfile [] = []
removeallfile ((Dir name ls):rest) =(Dir name ls):(removeallfile rest)
removeallfile (x:rest) = removeallfile rest

createTree commands (Dir name files)
    | commands == [] = (Dir name files)
    | (com!!1) == "cd" = if(com!!2=="..") then (Dir name files) else createTree (drop (aux rest 1 0) rest) (Dir name ((createTree rest (create (com!!2) files)):(remove (com!!2) files)))
    | (com!!1)=="ls" = createTree (dropWhile (\x->(x!!0)/="$") rest) (Dir name (addFiles files (takeWhile (\x->(x!!0)/="$") rest)))
    where 
        com = head commands
        rest = tail commands

aux _ 0 k = k
aux [] _ k = k
aux (x:xs) n k 
    |x!!1=="cd" && x!!2==".." = aux xs (n-1) (k+1)
    |x!!1=="cd" = aux xs (n+1) (k+1)
    |otherwise = aux xs n (k+1)

addFiles oldfiles newfiles 
    |newfiles ==[] = oldfiles
    |(file)!!0 =="dir" = addFiles ((create (file!!1) oldfiles):(remove (file!!1) oldfiles)) (tail newfiles)
    |otherwise =addFiles ((File (readInt (file!!0)) (file!!1)):oldfiles) (tail newfiles)
    where file = head newfiles

create dir [] = Dir dir []
create dir ((Dir name ls):rest) = if(dir==name)then (Dir name ls) else (create dir rest)
create dir (x:rest) = create dir rest

remove dir [] = []
remove dir ((Dir name subtree):rest) = if(dir==name)then rest else (Dir name subtree):(remove dir rest)
remove dir (x:rest)= [x]++ (remove dir rest)