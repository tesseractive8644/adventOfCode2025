import System.IO
import Control.Monad
import Data.List
import Data.Bifunctor
import Data.Bool
import Distribution.Simple.Utils (xargs)
import GHC.Num (integerSqr)

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

-- splitAt' l x xs will split xs into length x strings
-- Pre: x is a factor of l
splitAt' :: Int -> Int -> String -> [String]
splitAt' l x xs
 | l >= x = pref : splitAt' (l - x) x suff
 | otherwise = []
 where
    (pref, suff) = splitAt x xs

equal' :: [String] -> Bool
equal' [] = True
equal' [x] = True
equal' (x:xs@(xsx:_)) = x == xsx && equal' xs

invalid :: String -> Bool
invalid xs = or [equal' (splitAt' len x xs) | x <- [1..(len `div` 2)], len `mod` x == 0]
 where
    len = length xs

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