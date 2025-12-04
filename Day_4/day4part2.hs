import System.IO
import Control.Monad
import Data.List
import Data.Bool

step :: [String] -> Int
step grid
 | amount == 0 = 0
 | otherwise = amount + step (zipWith (zipWith (`bool` '.')) grid accessible)
 where
    intGrid = fmap (fromEnum .(== '@')) <$> grid
    horAdj = liftA2 (zipWith (+)) (0:) (flip (++) [0] . tail) <$> intGrid -- bad efficiency :/
    verAdj = transpose $ liftA2 (zipWith (+)) (0:) (flip (++) [0] . tail) <$> transpose intGrid
    diagAdj = transpose $ liftA2 (zipWith (+)) (0:) (flip (++) [0] . tail) <$> transpose horAdj
    adj = zipWith (zipWith (+)) diagAdj (zipWith (zipWith (+)) verAdj horAdj)
    accessible = zipWith (zipWith (\x y -> x == '@' && y < 4)) grid adj
    amount = sum $ sum . fmap fromEnum <$> accessible

solve :: Handle -> Handle -> IO()
solve inputStream outputStream = do
    doc <- hGetContents inputStream
    let grid = lines doc
    hPrint outputStream $ step grid

main :: IO()
main = do
    inputHandle <- openFile "in.txt" ReadMode
    outputHandle <- openFile "out.txt" WriteMode
    solve inputHandle outputHandle
    hClose inputHandle
    hClose outputHandle