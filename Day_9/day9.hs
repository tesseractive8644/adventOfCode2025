import System.IO
import Control.Monad
import Data.Bifunctor

area :: (Int, Int) -> (Int, Int) -> Int
area (x1,y1) (x2,y2) = abs (x1 - x2 + 1) * abs (y1 - y2 + 1)

solve :: Handle -> Handle -> IO()
solve inputStream outputStream = do
    doc <- hGetContents inputStream
    let xs :: [(Int,Int)] = bimap read (read . tail) . break (== ',') <$> lines doc
    hPrint outputStream $ maximum $ area <$> xs <*> xs

main :: IO()
main = do
    inputHandle <- openFile "in.txt" ReadMode
    outputHandle <- openFile "out.txt" WriteMode
    solve inputHandle outputHandle
    hClose inputHandle
    hClose outputHandle