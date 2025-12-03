import System.IO
import Control.Monad
import Data.List
import Data.Maybe

solve :: Handle -> Handle -> IO()
solve inputStream outputStream = do
    lns <- hGetContents inputStream
    let xs :: [[Int]] = fmap (read . singleton) <$> lines lns
    let maxis = foldr max 0 . init <$> xs -- urghhh
    let posis = (1+) . fromJust <$> zipWith elemIndex maxis xs
    let sndMaxis = foldr max 0 <$> zipWith drop posis xs
    hPrint outputStream $ sum $ zipWith (\x y -> 10*x + y) maxis sndMaxis

main :: IO()
main = do
    inputHandle <- openFile "in.txt" ReadMode
    outputHandle <- openFile "out.txt" WriteMode
    solve inputHandle outputHandle
    hClose inputHandle
    hClose outputHandle