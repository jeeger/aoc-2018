{-# LANGUAGE LambdaCase #-}
module Main where

import qualified Data.Map as M

hasNplicate:: Int -> String -> Int
hasNplicate n s = go s M.empty where
  go [] m = if n `elem` (M.elems m) then
              1
            else
              0
  go (c:rest) m = go rest $ M.alter (\case
                                        Nothing -> Just $ 1
                                        Just count -> Just $ count + 1)
                  c m

checksum1:: String -> Int
checksum1 = hasNplicate 2

checksum2:: String -> Int
checksum2 = hasNplicate 3

main:: IO ()
main = do
  ls <- getContents
  let ck1 = foldr (+) 0 $ map checksum1 $ lines ls
  let ck2 = foldr (+) 0 $ map checksum2 $ lines ls
  print (ck1 * ck2)
