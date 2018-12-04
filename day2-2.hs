{-# LANGUAGE LambdaCase #-}
module Main where

import Data.List(delete, intersect)

differsByOne:: String -> String -> Bool
differsByOne s1 s2 = go s1 s2 0 where
  go [] [] n = n == 1
  go (c1:cs1) (c2:cs2) n | c1 == c2 = go cs1 cs2 n
                         | otherwise = if n == 0 then
                                         go cs1 cs2 1
                                       else
                                         False

main:: IO ()
main = do
  ls <- getContents
  let diff = [(id1, id2) | id1 <- lines ls, id2 <- tail $ dropWhile (/=id1) (lines ls), differsByOne id1 id2]
  let (id1, id2) = head diff
  print $ id1 `intersect` id2
