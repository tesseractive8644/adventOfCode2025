import System.IO
import Control.Monad
import Data.Bifunctor
import Data.List

rangeUnion :: (Int, Int) -> [(Int, Int)] -> [(Int, Int)]
rangeUnion x [] = [x]
rangeUnion x@(xl,xr) y@((yl,yr):ys)
 | xr < yl = x : y
 | otherwise = (min xl yl, max xr yr):ys

rangeSize :: (Int, Int) -> Int
rangeSize (x,y) = y - x + 1

solve :: Handle -> Handle -> IO()
solve inputStream outputStream =
    hGetContents inputStream >>=
    hPrint outputStream . foldr ((+) . rangeSize) 0 . foldr rangeUnion [] . sort . (bimap read (read . tail) . break (== '-') <$>) . fst . break (== "") . lines

main :: IO()
main = do
    inputHandle <- openFile "in.txt" ReadMode
    outputHandle <- openFile "out.txt" WriteMode
    solve inputHandle outputHandle
    hClose inputHandle
    hClose outputHandle