import System.IO
import Control.Monad

data Cell = Split | Lines Int deriving Show

toCell :: Char -> Cell
toCell '^' = Split
toCell '.' = Lines 0
toCell 'S' = Lines 1

fromCell :: Cell -> Int
fromCell (Lines x) = x
fromCell _ = 0

-- Assumption: Same Length
compNext :: [Cell] -> [Cell] -> [Cell]
compNext [] [] = []
compNext [x] [y] = [x]
compNext (Lines x1 : Lines x2 : Lines x3 :xs) (_ : Split : ys) = Lines (x1 + x2) : Split : compNext (Lines (x2 + x3) : xs) ys
compNext (Lines x : xs) (Lines y : ys) = Lines x : compNext xs ys
compNext (_ : xs) (_ : ys) = Lines 0 : compNext xs ys

solve :: Handle -> Handle -> IO()
solve inputStream outputStream = do
    doc <- hGetContents inputStream
    let sim = scanl1 compNext $ fmap toCell <$> lines doc
    hPrint outputStream $ sum $ fromCell <$> last sim

main :: IO()
main = do
    inputHandle <- openFile "in.txt" ReadMode
    outputHandle <- openFile "out.txt" WriteMode
    solve inputHandle outputHandle
    hClose inputHandle
    hClose outputHandle