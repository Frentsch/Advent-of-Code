import System.IO
main = do
    contents <- readFile "input.txt"
    let line = lines contents
    let stacks = map reverse (take 9 line)
    let moves = (map (map readInt) (map words (drop 9 line)))
    print (part stacks (id) moves)
    print (part stacks (reverse) moves)

readInt :: String -> Int
readInt = read

part s f m 
 |m == [] = s
 |otherwise = part (move s f (head m)) f (tail m)

move s f (a:b:c:[]) 
 |c>b =(t (b-1) s )++[(d a (s!!(b-1)))] ++ (t (c-b-1) (d b s)) ++[(f (t a (s!!(b-1)))) ++ (s!!(c-1)) ] ++ (d c s)
 |c<b= (t (c-1) s )++[(f(t a (s!!(b-1)))) ++(s!!(c-1))]++ (t (b-c-1) (d c s)) ++[(d a (s!!(b-1)))]  ++ (d b s)
 |otherwise = s
 where 
    t=take
    d=drop