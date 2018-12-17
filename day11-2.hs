{-# LANGUAGE OverloadedStrings #-}

module Main where

import Formatting
import Data.Foldable(foldr')
  
import Data.Maybe (fromMaybe)
import Data.List(maximumBy)
import qualified Data.HashMap.Strict as M
import Data.HashMap.Strict((!))

import qualified Data.Text as T
import Data.Text.IO

gSize:: Int
gSize = 300

gSerial:: Int
gSerial = 9110

powerlevel:: Int -> (Int, Int) -> Int
powerlevel serial (x, y) | x < 0 || x > gSize || y < 0 || x > gSize = 0
                         | otherwise = let rackid = x + 10
                                           powerlevel = (rackid * y + serial) * rackid
                                           hundreds x | x < 100 = 0
                                                      | otherwise = (x `mod` 1000) `div` 100 in
                                         hundreds powerlevel - 5

summedTable:: Int -> M.HashMap (Int, Int) Int
summedTable serial = foldl calculate M.empty [(x, y) | y <- [1..gSize], x <- [1..gSize]] where
  calculate m (x, y) = M.insert (x, y) (powerlevel serial (x, y) + safeLookup (x, y-1) m + safeLookup (x-1, y) m - safeLookup (x-1, y-1) m) m

safeLookup:: (Int, Int) -> M.HashMap (Int, Int) Int -> Int
safeLookup (x, y) m | x < 1 || x > gSize || y < 1 || y > gSize = 0
                    | otherwise = fromMaybe (error $ "Key " ++ show (x, y) ++ " not found.") (M.lookup (x, y) m)

comparePower:: (Int, Int, Int, Int) -> (Int, Int, Int, Int) -> Ordering
comparePower (_, _, _, p1) (_, _, _, p2) = compare p1 p2

showPower:: Int -> T.Text
showPower serial = T.unlines [T.unwords [sformat ((left 4 ' ' %. int) % " ") (powerlevel serial (x, y)) | x <- [1..gSize]] | y <- [1..gSize]]

showMap:: M.HashMap (Int, Int) Int -> T.Text
showMap t = T.unlines [T.unwords [sformat ((left 4 ' ' %. int) % " ") (M.lookupDefault 0 (x, y) t) | x <- [1..gSize]] | y <- [1..gSize]]

main:: IO ()
main =  let t = summedTable gSerial
            summedPower (x,y) size = safeLookup (x + (size - 1), y + (size - 1)) t + safeLookup (x,y) t - safeLookup (x, y + (size - 1)) t - safeLookup (x+(size - 1), y) t in
          print $ maximumBy comparePower [(x + 1, y + 1, size - 1, summedPower (x, y) size) | size <- [1..gSize], x <- [1..(gSize - size)], y <- [1..(gSize - size)]]
