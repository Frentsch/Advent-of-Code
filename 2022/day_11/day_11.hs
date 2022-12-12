import System.IO
import Data.Char
import Data.List
import Prelude
main = do
    --let objects = [[79, 98], [54, 65, 75, 74], [79, 60, 97], [74]]
    --let ops = [(* 19), (+ 6), (\x->x*x), (+ 3)]
    --let test = [(\x-> mod x 23==0),(\x-> mod x 19==0),(\x-> mod x 13==0),(\x-> mod x 17==0)]
    --let next= [(2,3),(2,0),(1,3), (0,1)]
    let objects = [[52, 60, 85, 69, 75, 75], [96, 82, 61, 99, 82, 84, 85], [95, 79], [88, 50, 82, 65, 77],[66, 90, 59, 90, 87, 63, 53, 88],[92, 75, 62],[94, 86, 76, 67],[57]]
    let iobjects = map (map toInteger) objects
    let ops = [(* (toInteger 17)), (+ (toInteger 8)), (+ (toInteger 6)), (* (toInteger 19)),(+ (toInteger 7)),(\x->x*x), (+ (toInteger 1)),(+ (toInteger 2))]
    let test = [(\x-> mod x 13==0),(\x-> mod x 7==0),(\x-> mod x 19==0),(\x-> mod x 2==0),(\x-> mod x 5==0),(\x-> mod x 3==0),(\x-> mod x 11==0),(\x-> mod x 17==0)]
    let next= [(6,7),(0,7),(5,3), (4,1), (1,0), (3,4), (5,2), (6,2)]
    print (execute 10000 iobjects ops test next [0 | y<-[0..7]])

readInt :: String -> Int
readInt = read 

multmax ins = (sorted!!0) * (sorted!!1) 
    where 
        sorted = reverse (sort ins)

execute :: Int->[[Integer]]->[(Integer->Integer)]->[(Integer->Bool)]->[(Int,Int)]->[Integer]->Integer
execute 0 objects ops test next inspections= multmax inspections
execute roundn objects ops test next inspections = execute (roundn-1) (tail res) ops test next (zipWith (+) inspections (head res))
    where
        res = mround objects ops test next 0 []

mround :: [[Integer]]->[(Integer->Integer)]->[(Integer->Bool)]->[(Int,Int)]->Int->[Integer]->[[Integer]]
mround objects ops test next monkey inspections
    |monkey==length(objects)=(inspections:objects)
    |otherwise = mround (mmove (objects!!monkey) ((take monkey objects)++[[]]++(drop (monkey+1) objects)) (ops!!monkey) (test!!monkey) (next!!monkey)) ops test next (monkey+1) (inspections ++[toInteger (length (objects!!monkey))])

mmove [] objects op test next = objects
mmove (x:xs) objects op test next = mmove xs ((take nextmonkey objects) ++ [((objects!!nextmonkey) ++ [mod (op x) (17*11*3*5*2*19*7*13)])] ++ (drop (nextmonkey+1) objects)) op test next
    where
        nextmonkey = if(test ((op x))) then fst next else snd next