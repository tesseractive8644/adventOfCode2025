import System.IO
import Control.Monad
import Data.List
import Data.Map qualified as Map
import Data.Maybe
import Data.Tuple (swap)
import Data.Ord
import GHC.Float

newtype Point = Point [Float] deriving (Eq, Ord, Show)

xVal :: Point -> Int
xVal (Point [x,y,z]) = round x

dist :: (Point, Point) -> Float
dist (Point [x1,y1,z1], Point [x2,y2,z2]) = sqrt $ (x1 - x2) ^ 2 + (y1 - y2) ^ 2 + (z1 - z2) ^ 2

toPoint :: [String] -> Point
toPoint [x,y,z] = Point [read x, read y, read z]

split :: Eq a => a -> [a] -> [[a]]
split _ [] = [[]]
split c (x:xs)
 | c == x = [] : r : rs
 | otherwise = (x : r) : rs
 where
    (r:rs) = split c xs

component :: [(Point, Point)] -> Point -> Point
component adjl x
 | null nxts = x
 | otherwise = component adjl $ head nxts
 where
    nxts = [v | (u,v) <- adjl, x == u]

merge :: [(Point, Point)] -> (Point, Point) -> [(Point, Point)]
merge adjl (x,y)
 | tx == ty = adjl
 | otherwise = (tx,ty) : adjl
 where
    (tx,ty) = (component adjl x, component adjl y)

runDsu :: [Point] -> [(Point, Point)] -> [(Point, Point)] -> Int
runDsu pts (e@(u,v):adjl) dsu
 | component dsu u == component dsu v = runDsu pts adjl dsu
 | length comps == 1 = xVal u * xVal v
 | otherwise = runDsu pts adjl dsu'
 where
    dsu' = merge dsu e
    comps = freqArr $ component dsu' <$> pts

freqArr :: forall a . Ord a => [a] -> [(a, Int)]
freqArr = Map.toList . genMap
 where
    genMap :: [a] -> Map.Map a Int
    genMap [] = Map.empty
    genMap (x:xs)
     | isNothing t = Map.insert x 1 $ nxt
     | otherwise = Map.adjust (+1) x nxt
     where
        nxt = genMap xs
        t = Map.lookup x nxt

solve :: Handle -> Handle -> IO()
solve inputStream outputStream = do
    doc <- hGetContents inputStream
    let pts = toPoint . split ',' <$> lines doc
    let ptPairs = [(x,y) | x <- pts, y <- pts, x < y]
    let ptPairDists = dist <$> ptPairs
    let pairs = sort $ zip ptPairDists ptPairs
    hPrint outputStream $ runDsu pts (snd <$> pairs) []

main :: IO()
main = do
    inputHandle <- openFile "in.txt" ReadMode
    outputHandle <- openFile "out.txt" WriteMode
    solve inputHandle outputHandle
    hClose inputHandle
    hClose outputHandle