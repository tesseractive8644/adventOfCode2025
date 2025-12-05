import System.IO
import Control.Monad
import Data.Bifunctor

solve :: Handle -> Handle -> IO()
solve inputStream outputStream = do
    doc <- hGetContents inputStream
    let (ranges, "":ints) = break (== "") $ lines doc
    let pairs :: [(Int,Int)] = bimap read (read . tail) . break (== '-') <$> ranges
    let valid = [x | x <- read <$> ints, not $ null $ [a | (a,b) <- pairs, a <= x, x <= b]]
    hPrint outputStream $ length valid

main :: IO()
main = do
    inputHandle <- openFile "in.txt" ReadMode
    outputHandle <- openFile "out.txt" WriteMode
    solve inputHandle outputHandle
    hClose inputHandle
    hClose outputHandle