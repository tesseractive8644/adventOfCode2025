import System.IO
import Control.Monad

-- Assumption: Same Length
compNext :: String -> String -> String
compNext "" "" = ""
compNext [x] [y] = [x]
compNext (x1:xss@(x2:xs)) (y1:yss@(y2:ys))
 | x2 == 'S' && y2 == '^' = 'S' : '^' : 'S' : compNext (tail xs) (tail ys)
 | x1 == 'S' && y1 == '^' = '^' : 'S' : compNext xs ys
 | x1 == 'S' = 'S' : compNext xss yss
 | otherwise = y1 : compNext xss yss

findSplits :: String -> String -> [Int]
findSplits "" "" = []
findSplits (x:xs) (y:ys) = fromEnum (x == 'S' && y == '^') : findSplits xs ys

solve :: Handle -> Handle -> IO()
solve inputStream outputStream = do
    doc <- hGetContents inputStream
    let sim = scanl1 compNext $ lines doc
    hPrint outputStream $ sum $ sum <$> zipWith findSplits sim (tail sim)

main :: IO()
main = do
    inputHandle <- openFile "in.txt" ReadMode
    outputHandle <- openFile "out.txt" WriteMode
    solve inputHandle outputHandle
    hClose inputHandle
    hClose outputHandle