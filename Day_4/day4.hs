import System.IO
import Control.Monad
import Data.List

solve :: Handle -> Handle -> IO()
solve inputStream outputStream = do
    doc <- hGetContents inputStream
    let grid = lines doc
    let intGrid :: [[Int]] = fmap (fromEnum .(== '@')) <$> grid
    let horAdj = liftA2 (zipWith (+)) (0:) (flip (++) [0] . tail) <$> intGrid -- bad efficiency :/
    let verAdj = transpose $ liftA2 (zipWith (+)) (0:) (flip (++) [0] . tail) <$> transpose intGrid
    let diagAdj = transpose $ liftA2 (zipWith (+)) (0:) (flip (++) [0] . tail) <$> transpose horAdj
    let adj = zipWith (zipWith (+)) diagAdj (zipWith (zipWith (+)) verAdj horAdj)
    let accessible = zipWith (zipWith (\x y -> x == '@' && y < 4)) grid adj
    hPrint outputStream $ sum $ sum . fmap fromEnum <$> accessible

main :: IO()
main = do
    inputHandle <- openFile "in.txt" ReadMode
    outputHandle <- openFile "out.txt" WriteMode
    solve inputHandle outputHandle
    hClose inputHandle
    hClose outputHandle