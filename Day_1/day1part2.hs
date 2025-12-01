import System.IO
import Control.Monad
import Data.List
import Data.Bool

turnAmount :: Char -> Int -> Int
turnAmount 'L' = negate
turnAmount _ = id

foldZero :: [Int] -> Int -> Int
foldZero [] _ = 0
foldZero (x:xs) 0
 | x < 0 = foldZero ((x+1):xs) 99
 | x > 0 = foldZero ((x-1):xs) 1
foldZero (x:xs) d
 | d + x > 100 = 1 + foldZero ((x - diffPos) : xs) ((d + diffPos + 100) `mod` 100)
 | d + x < 0 = 1 + foldZero ((x - diffNeg) : xs) ((d + diffNeg + 100) `mod` 100)
 | d + x == 0 || d + x == 100 = 1 + foldZero xs 0
 | otherwise = foldZero xs (d + x)
 where
    diffPos = min x 100
    diffNeg = max x (-100)

solve :: Handle -> Handle -> IO()
solve inputStream outputStream = do
    cs <- hGetContents inputStream
    let (dir, mag :: [Int]) = unzip [(i, read is) | (i:is) <- lines cs]
    let xs = zipWith turnAmount dir mag
    hPrint outputStream $ foldZero xs 50

main :: IO()
main = do
    inputHandle <- openFile "in.txt" ReadMode
    outputHandle <- openFile "out.txt" WriteMode
    solve inputHandle outputHandle
    hClose inputHandle
    hClose outputHandle