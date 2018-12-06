{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where 
import Prelude hiding(getContents)

import Data.Attoparsec.Text
import Data.Text.IO
import Data.Text hiding(length, foldr, head, tail, empty, minimum)
import Text.Pretty.Simple (pPrint)
import Data.HashMap.Strict hiding (foldr)
import GHC.Generics(Generic)
import Data.Hashable
import Data.Monoid
import Data.Function(on)
import Data.Foldable(minimumBy)

data Coordinate = Coord { cx:: Int
                        , cy:: Int
                        } deriving (Show, Eq, Generic)

instance Hashable Coordinate

data Area = Area { size:: Int
                 , finite:: Bool
                 } deriving (Show, Eq)

newtype AssignedArea = AssignedArea (HashMap Coordinate Area) deriving (Show, Eq)


instance Ord Area where
  compare (Area s1 f1) (Area s2 f2) = compare f1 f2 <> compare s1 s2

parseCoordinates:: Parser Coordinate
parseCoordinates = do
  x <- decimal
  char ','
  space
  y <- decimal
  endOfLine
  return $ Coord x y
  <?> "parseCoordinates"

parseAll:: Parser [Coordinate]
parseAll = do
  coords <- many1 parseCoordinates
  endOfInput
  return coords
  <?> "parseAll"

emptyArea:: AssignedArea
emptyArea = AssignedArea empty

boundingBox:: [Coordinate] -> (Int, Int, Int, Int)
boundingBox c = (minx, maxx, miny, maxy) where
  minx = calc min cx
  maxx = calc max cx
  miny = calc min cy
  maxy = calc max cy
  calc f1 f2 = foldr (f1 . f2) (f2 $ head c) (tail c)

mdist:: Coordinate -> Coordinate -> Int
mdist (Coord x1 y1) (Coord x2 y2) = abs (x1 - x2) + abs (y1 - y2)

argmax:: Ord a => (k, a) -> HashMap k a -> (k, a)
argmax init m = foldrWithKey argmax' init m where
  argmax' k v (mk, mv) = if v > mv then
                           (k, v)
                         else
                           (mk, mv)

findClosestCoordinate:: Coordinate -> [Coordinate] -> Coordinate
findClosestCoordinate c1 cs = fst $ minimumBy (compare `on` snd) (fmap (\c -> (c, mdist c1 c)) cs)

mergeAreas:: Area -> Area -> Area
mergeAreas (Area s1 f1) (Area s2 f2) = Area (s1 + s2) (f1 && f2)

calculateAreas:: [Coordinate] -> AssignedArea
calculateAreas cs = foldr updateArea emptyArea [Coord x y | x <- [bx1..bx2], y <- [by1..by2]]  where
  (bx1, bx2, by1, by2) = boundingBox cs
  finite (Coord x y) = not (x == bx1 || x == bx2 || y == by1 || y == by2)
  updateArea c (AssignedArea m) = AssignedArea $ insertWith mergeAreas (findClosestCoordinate c cs) (Area 1 (finite c)) m

findBiggestArea:: AssignedArea -> Area
findBiggestArea (AssignedArea m) = snd $ argmax (Coord 0 0, Area 0 True) m
  
main = do
  lines <- getContents
  case parseOnly parseAll lines of
    Left error -> print error
    Right coords -> do
      let (minx, maxx, miny, maxy) = boundingBox coords
      pPrint $ findBiggestArea $ calculateAreas coords 
