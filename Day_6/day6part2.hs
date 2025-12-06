import System.IO
import Control.Monad
import Data.List

data Operation = Val Int | Mul | Add deriving Show

parse :: [String] -> [Operation]
parse [] = []
parse (str:strs)
 | null (words str) = parse strs
 | head rts == '*' = Mul : Val (read $ reverse $ tail rts) : parse strs
 | head rts == '+' = Add : Val (read $ reverse $ tail rts) : parse strs
 | otherwise = Val (read str) : parse strs
 where
    rts = reverse str

calc :: Int -> (Int -> Int -> Int) -> [Operation] -> [Int]
calc acc _ [] = [acc]
calc acc comb (Val x : ops) = calc (comb acc x) comb ops
calc acc _ (Add : ops) = acc : calc 0 (+) ops
calc acc _ (Mul : ops) = acc : calc 1 (*) ops

solve :: Handle -> Handle -> IO()
solve inputStream outputStream = hGetContents inputStream >>=
    hPrint outputStream . sum . calc 0 (+) . parse . transpose . lines

main :: IO()
main = do
    inputHandle <- openFile "in.txt" ReadMode
    outputHandle <- openFile "out.txt" WriteMode
    solve inputHandle outputHandle
    hClose inputHandle
    hClose outputHandle