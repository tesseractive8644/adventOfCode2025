import System.IO
import Control.Monad
import Data.List
import Data.Maybe

findDigits :: [Int] -> [Int]
findDigits xs = findDigits' xs 12
 where
    findDigits' :: [Int] -> Int -> [Int]
    findDigits' _ 0 = []
    findDigits' xs x = maxi : findDigits' rem (x - 1)
     where
        scope = (reverse . drop (x - 1) . reverse) xs
        maxi = foldr max 0 scope
        posi = fromJust $ elemIndex maxi scope
        rem = drop (posi + 1) xs

convertDigits :: [Int] -> Int
convertDigits = foldl (\x y -> 10 * x + y) 0

solve :: Handle -> Handle -> IO()
solve inputStream outputStream = do
    lns <- hGetContents inputStream
    let xs :: [[Int]] = fmap (read . singleton) <$> lines lns
    hPrint outputStream $ sum $ convertDigits . findDigits <$> xs

main :: IO()
main = do
    inputHandle <- openFile "in.txt" ReadMode
    outputHandle <- openFile "out.txt" WriteMode
    solve inputHandle outputHandle
    hClose inputHandle
    hClose outputHandle