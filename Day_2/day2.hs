import System.IO
import Control.Monad
import Data.List
import Data.Bifunctor
import Data.Bool

-- In Data.List.Split but not importable as I have not downloaded it
splitOn :: Eq a => a -> [a] -> [[a]]
splitOn _ [] = [[]]
splitOn c (x:xs)
 | c == x = [] : rem
 | otherwise = (x : r) : em
 where
    rem@(r:em) = splitOn c xs

readPair :: (String, String) -> (Int, Int)
readPair (x,y) = (read x :: Int, read y :: Int)

invalid :: String -> Bool
invalid x
 | len `mod` 2 == 1 = False
 | otherwise = uncurry (==) $ splitAt (len `div` 2) x
 where
    len = length x

testAll :: (Int, Int) -> Int
testAll (i,j) = sum [bool 0 x (invalid (show x)) | x <- [i..j]]

solve :: Handle -> Handle -> IO()
solve inputStream outputStream = do
    doc <- hGetContents inputStream
    let cs = map (break (=='-')) (splitOn ',' doc)
    let xs = fmap (readPair . second tail) cs
    hPrint outputStream (sum $ fmap testAll xs)

main :: IO()
main = do
    inputHandle <- openFile "in.txt" ReadMode
    outputHandle <- openFile "out.txt" WriteMode
    solve inputHandle outputHandle
    hClose inputHandle
    hClose outputHandle