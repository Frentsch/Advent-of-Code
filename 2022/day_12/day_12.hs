import System.IO
import Data.Char
import Data.List
import Prelude
main = do
    input <- readFile("input.txt")
    let line = lines input
    print line