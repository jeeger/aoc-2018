{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module Main where

import Data.Attoparsec.Text(Parser, decimal, char, space, endOfLine, many1, parseOnly)
import Data.Text(Text)
import GHC.Generics(Generic)
import Data.Hashable
import Data.Text.IO(getContents)
import qualified Data.HashMap.Strict as M

data Claim = Claim { claimid:: Int
                   , cx:: Int
                   , cy:: Int
                   , cxsize:: Int
                   , cysize:: Int
                   } deriving (Show, Eq, Ord, Generic)

instance Hashable Claim

newtype ClaimStore = ClaimStore (M.HashMap Claim Int)

empty:: ClaimStore
empty = ClaimStore $ M.empty

parseClaim:: Parser Claim
parseClaim = do
  char '#'
  claim <- decimal
  space
  char '@'
  space
  xpos <- decimal
  char ','
  ypos <- decimal
  char ':'
  space
  xsize <- decimal
  char 'x'
  ysize <- decimal
  endOfLine
  return $ Claim claim xpos ypos xsize ysize

parseAllClaims:: Parser [Claim]
parseAllClaims = many1 parseClaim

cx2:: Claim -> Int
cx2 c = cx c + cxsize c - 1

cy2:: Claim -> Int
cy2 c = cy c + cysize c - 1

claimsOverlap:: Claim -> Claim -> Bool
claimsOverlap c1 c2 = not (left || right || above || below) -- We just code the cases where there is *no* overlap.
  where left = cx c1 < cx c2 && cx c1 + cxsize c1 <= cx c2
        right = cx c1 >= cx c2 + cxsize c2 -- Since size > 0, don't need to check the second coordinate.
        above = cy c1 >= cy c2 + cysize c2 -- Since size > 0, don't need to check the second coordinate
        below = cy c1 < cy c2 && cy c1 + cysize c1 <= cy c2

insertClaim:: ClaimStore -> Claim -> ClaimStore
insertClaim (ClaimStore s) c = let increment k v = if claimsOverlap k c then v + 1 else v
                                   overlapping = filter (claimsOverlap c) (M.keys s)
                                   incremented = M.mapWithKey increment s in
                                 ClaimStore $ M.insert c (length overlapping) incremented

findNonoverlapped:: ClaimStore -> [Claim]
findNonoverlapped (ClaimStore s) = M.keys $ M.filter (\x -> x == 0) s
  
main = do
  lines <- Data.Text.IO.getContents
  case parseOnly parseAllClaims lines of
    Left error -> print error
    Right claims -> do
      let overlapMap = foldr (flip insertClaim) empty claims
      print $ findNonoverlapped overlapMap
