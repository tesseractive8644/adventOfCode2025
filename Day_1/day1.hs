import System.IO
import Control.Monad
import Data.List
import Data.Bool

turnAmount :: Char -> Int -> Int
turnAmount 'L' = negate
turnAmount _ = id

incrIfMatch :: Int -> Int -> Int -> Int
incrIfMatch x y
 | x == y = succ
 | otherwise = id

solve :: Handle -> Handle -> IO()
solve inputStream outputStream = do
    cs <- hGetContents inputStream
    let (dir, mag :: [Int]) = unzip [(i, read is) | (i:is) <- lines cs]
    let xs = zipWith turnAmount dir mag
    hPrint outputStream $ foldr (incrIfMatch 0 . (`mod` 100)) 0 $ scanl (+) 50 xs

main :: IO()
main = do
    inputHandle <- openFile "in.txt" ReadMode
    outputHandle <- openFile "out.txt" WriteMode
    solve inputHandle outputHandle
    hClose inputHandle
    hClose outputHandle