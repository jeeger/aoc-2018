{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Attoparsec.Text(Parser, decimal, char, space, endOfLine, many1, parseOnly)
import Data.Text(Text)
import Data.Text.IO(getContents)
import qualified Data.HashMap.Strict as M

data Claim = Claim { claimid:: Int
                   , cx:: Int
                   , cy:: Int
                   , cxsize:: Int
                   , cysize:: Int
                   } deriving (Show, Eq, Ord)

data TapestryState = Uncovered
                   | Covered
                   | Overlapped
                   deriving (Show)

newtype Tapestry = Tapestry (M.HashMap (Int, Int) TapestryState)
                 deriving (Show)
  

-- data QuadTree = TreeNode { size:: Int
--                          , x :: Int
--                          , y :: Int
--                          , nw :: QuadTree
--                          , ne :: QuadTree
--                          , se :: QuadTree
--                          , sw :: QuadTree
--                          }
--               | TreeLeaf { size :: Int
--                          , x :: Int
--                          , y :: Int
--                          , children :: [Claim]
--                          }

-- empty:: Int -> QuadTree
-- empty size = TreeLeaf size 0 0 []

-- intersects:: Claim -> QuadTree -> Bool
-- intersects _ TreeLeaf{} = True
-- intersects c qt@TreeNode{} = (cx 
--                              (cy c > y qt && cy c < (y qt + size qt))

-- insert:: Claim -> QuadTree -> QuadTree
-- insert c l@(TreeLeaf {}) = if length (children l) < 4 then
--                              l {children = c: children l}
--                            else
--                              error "Splitting undefined"
-- insert c l@{TreeNode {}) = l {

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

unmarked :: Int -> Tapestry
unmarked size = Tapestry $ M.empty

markClaim :: Tapestry -> Claim -> Tapestry
markClaim (Tapestry m) c = Tapestry $ M.unionWith merge m (M.fromList [((x,y), Covered) | x <- [(cx c)..(cx2 c)], y <- [(cy c)..(cy2 c)]]) where
  merge Covered Covered = Overlapped
  merge Overlapped Covered = Overlapped

overlappedInches :: Tapestry -> Int
overlappedInches (Tapestry a) = foldr add 0 a where
  add Covered c = c
  add Overlapped c = c + 1
  
main = do
  lines <- Data.Text.IO.getContents
  case parseOnly parseAllClaims lines of
    Left error -> print error
    Right claims -> do
      let marked = foldr (flip markClaim) (unmarked 1000) claims
      print $ overlappedInches marked
  
        
