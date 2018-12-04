module Main where

import qualified Data.Set as S

readplus:: String -> Int
readplus ('+':rest) = read rest
readplus s = read s

firstDup:: Ord a => [a] -> a
firstDup nums = go nums S.empty where
  go [] s = error "No duplicates."
  go (n:ns) s = if n `S.member` s then
                  n
                else
                  go ns $ S.insert n s

main :: IO ()
main = do
  ls <- getContents
  let freqs = scanl (+) 0 $ cycle $ map readplus $ lines ls
  print $ firstDup freqs
