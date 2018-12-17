module Main where

import Text.Pretty.Simple (pPrint)
import Data.Text.IO
import Data.Foldable(foldr)
import Data.Function(on)
import Data.List(maximumBy)

powerlevel:: Int -> (Int, Int) -> Int
powerlevel serial (x, y) = let rackid = x + 10
                               powerlevel = (rackid * y + serial) * rackid
                               hundreds x | x < 100 = 0
                                          | otherwise = (x `mod` 1000) `div` 100 in
                             hundreds powerlevel - 5

totalPower:: (Int,Int) -> Int -> Int
totalPower (x, y) serial = sum [powerlevel serial (x1,y1) | x1 <- [x..x+2], y1 <- [y..y+2]]

comparePower:: (Int, Int, Int) -> (Int, Int, Int) -> Ordering
comparePower (_, _, p1) (_, _, p2) = compare p1 p2

main:: IO ()
main =  print $ maximumBy comparePower [ (x,y, totalPower (x, y) 9110) | x <- [1..300], y <- [1..300]]
