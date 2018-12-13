{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}

module Main where
import Prelude hiding (getContents, putStr)

import Data.Attoparsec.Text hiding (take, takeWhile)
import Text.Pretty.Simple (pPrint)
import Data.Text.IO hiding (putStrLn)
import Data.Text(Text, pack, cons, unpack)
import Control.Concurrent (threadDelay)
import Text.Printf (printf)
import Data.List(nubBy, sort, groupBy)
import Data.Foldable(foldr)
import Data.Function(on)
import Data.Functor
import GHC.Generics(Generic)
import Data.Hashable
import Data.Set(fromList, member, Set)
import Data.Monoid ((<>))
import System.Console.ANSI(clearScreen)


data Point = Point { pPosition:: !(Int, Int)
                   , pVelocity:: !(Int, Int)
                   } deriving (Show, Eq)

instance Ord Point where
  compare = (compare `on` (snd . pPosition)) <> (compare `on` (fst . pPosition))

parsePair:: Parser (Int, Int)
parsePair = do
  string "<"
  skipSpace
  i1 <- signed decimal
  string ","
  skipSpace
  i2 <- signed decimal
  char '>'
  return (i1, i2)
  <?> "parsePair"

parsePoint:: Parser Point
parsePoint = do
  string "position="
  pos <- parsePair
  string " velocity="
  vel <- parsePair
  endOfLine
  return $ Point pos vel
  <?> "parsePoint"

parsePoints:: Parser [Point]
parsePoints = many1 parsePoint

stepPoints:: [Point] -> [Point]
stepPoints = map pointStep where
  pointStep (Point (posx, posy) (velx, vely)) = Point (posx + velx, posy + vely) (velx, vely)

pointContains:: Int -> (Int, Int) -> Set (Int, Int) -> Bool
pointContains radius (px1, py1) = any nearer where
  nearer (px2, py2) = ceiling (sqrt (fromIntegral (dx * dx + dy * dy))) < radius where
    dx = abs (px2 - px1)
    dy = abs (py2 - py1)

boundingBox:: [Point] -> ((Int, Int), (Int, Int))
boundingBox p = (foldr (coord min) (0, 0) p, foldr (coord max) (0, 0) p) where
  coord f (Point (posx, posy) _) (oposx, oposy) = (posx `f` oposx, posy `f` oposy)

printBox:: ((Int, Int), (Int, Int)) -> Int -> [Point] -> Text
printBox ((sx, sy), (mx, my)) scale p = printBox [sx,(sx + scale)..mx] [sy,(sy + scale)..my] where
  pointSet = fromList $ map pPosition p
  printBox [] (y1:ys) = '\n' `cons` printBox [sx,(sx + scale)..mx] ys
  printBox _ [] = ""
  printBox x@(x1:xs) y@(y1:ys) = if pointContains scale (x1, y1) pointSet then
                                   '#' `cons` printBox xs y
                                 else
                                   '.' `cons` printBox xs y

printPoints:: Int -> [Point] -> IO ()
printPoints scale p = do
  clearScreen
  putStr $ printBox (boundingBox p) scale p
  return ()

shrinking:: [Point] -> [Point] -> Bool
shrinking p1 p2 = dx1 > dx2 || dy1 > dy2 where
  ((sx1, sy1), (mx1, my1)) = boundingBox p1
  ((sx2, sy2), (mx2, my2)) = boundingBox p2
  dx1 = mx1 - sx1
  dx2 = mx2 - sx2
  dy1 = my1 - sy1
  dy2 = my2 - sy2

growingPoints:: ([Point] -> [Point]) -> [Point] -> [[Point]]
growingPoints f p = growingStep p (f p) where
  growingStep p1 p2 | shrinking p1 p2 = growingStep p2 (f p2)
                    | otherwise = p1: growingStep p2 (f p2)

shrinkingPoints:: ([Point] -> [Point]) -> [Point] -> [[Point]]
shrinkingPoints f p = shrinkingStep p (f p) where
  shrinkingStep p1 p2 | shrinking p1 p2 = p1 : shrinkingStep p2 (f p2)
                      | otherwise  = []

main:: IO ()
main = do
  lines <- getContents
  case parseOnly parsePoints lines of
         Left error -> print error
         Right points ->
           print $ length $ shrinkingPoints stepPoints points
