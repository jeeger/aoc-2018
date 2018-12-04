module Main where

readplus:: String -> Int
readplus ('+':rest) = read rest
readplus s = read s

main :: IO ()
main = do
  ls <- getContents
  print $ foldr (+) 0 $ map readplus $ lines ls
