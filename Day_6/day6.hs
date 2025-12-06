import System.IO
import Control.Monad
import Data.List

op :: String -> (Int -> Int -> Int)
op "*" = (*)
op "+" = (+)

solve :: Handle -> Handle -> IO()
solve inputStream outputStream = hGetContents inputStream >>=
    hPrint outputStream . sum . (liftA2 foldr1 (op . head) ((read <$>) . tail) <$>) . transpose . reverse . (words <$>) . lines

main :: IO()
main = do
    inputHandle <- openFile "in.txt" ReadMode
    outputHandle <- openFile "out.txt" WriteMode
    solve inputHandle outputHandle
    hClose inputHandle
    hClose outputHandle