{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}

module Main where
import Prelude hiding (getContents)

import Data.Attoparsec.Text hiding (take, takeWhile)
import Text.Pretty.Simple (pPrint)
import Data.Text.IO hiding (putStrLn)
import Data.List(sort, groupBy)
import Data.Foldable(foldr)
import Data.Function(on)
import Data.Functor
import GHC.Generics(Generic)
import Data.Hashable
import Data.Char(ord)
import qualified Data.HashMap.Strict as M
import Control.Applicative ((<|>))


newtype Marble = Marb { mVal:: Int } deriving (Eq)

instance Show Marble where
  show (Marb val) = " " ++ show val ++ " "

data MarbleCircle = Circ { mCircle:: [Marble]
                         , mCurrent:: Int
                         } deriving (Eq)


newtype Player = Player { pPlayer:: Int } deriving (Show, Eq, Generic)
instance Hashable Player

newtype Score = Score { sScore:: Int} deriving (Show, Eq, Ord)

data Players = Players { psPlayer:: M.HashMap Player Score
                       , psCurrent:: Player
                       , psMarbleValue:: Marble} deriving (Show, Eq)

instance Show MarbleCircle where
  show (Circ circ cur) = "«" ++ left ++ "[" ++ show curMarb ++ "]" ++ right ++ "»" where
    marbs = map show circ
    left = concat $ take cur marbs
    right = concat $ drop (cur + 1) marbs
    curMarb = circ !! cur
    

isWinning:: Marble -> Bool
isWinning (Marb val) = (val `mod` 23) == 0 && val > 1

emptyCircle:: MarbleCircle
emptyCircle = Circ [Marb 0] 0

placeMarble:: Marble -> MarbleCircle -> MarbleCircle
placeMarble marb (Circ c cur) = Circ (take newcur c ++ [marb] ++ drop newcur c) newcur where
  newcur = ((cur + 1) `mod` length c) + 1

takeMarble:: MarbleCircle -> (MarbleCircle, Marble)
takeMarble (Circ c cur) = (Circ (take totake c ++  drop (totake + 1) c) totake, c !! totake) where
  totake = (cur - 7) `mod` length c

makeMove:: Marble -> MarbleCircle -> (MarbleCircle, Score)
makeMove marb@(Marb mval) circ | isWinning marb = let (mc, Marb tval) = takeMarble circ in
                                   (mc, Score (tval + mval))
                               | otherwise = (placeMarble marb circ, Score 0)

emptyPlayers:: Int -> Players
emptyPlayers c = Players (M.fromList [(Player p, Score 0) | p <- [0..(c-1)]]) (Player 0) (Marb 1)

addScore:: Score -> Score -> Score
addScore (Score s1) (Score s2) = Score (s1 + s2)

nextMarble:: Marble -> Marble
nextMarble (Marb val) = Marb (val + 1)

nextPlayer:: Score -> Players -> Players
nextPlayer score p@(Players ps (Player current) marb) = p { psPlayer = M.insertWith addScore (psCurrent p) score ps
                                                                      , psCurrent = Player $ if lastPlayer then 0 else current + 1
                                                                      , psMarbleValue = nextMarble marb
                                                                      } where
  lastPlayer = (current + 1) == length ps


oneMove:: (Players, MarbleCircle) -> (Players, MarbleCircle)
oneMove (p, mc) = (nextPlayers, nextCirc) where
  (nextCirc, score) = makeMove (psMarbleValue p) mc
  nextPlayers = nextPlayer score p


parsePlay:: Parser (Int, Int)
parsePlay = do
  players <- decimal
  string " players; last marble is worth "
  marbles <- decimal
  string " points"
  return (players, marbles)

finishedGamestate:: Int -> [(Players, MarbleCircle)] -> Players
finishedGamestate maxmarble ((p@(Players _ _ (Marb mval)), circ):ps) = if mval <= maxmarble then
                                     finishedGamestate maxmarble ps
                                   else
                                     p

winningScore:: Players -> Score
winningScore (Players p _ _) = maximum (M.elems p)

main:: IO ()
main = do
  lines <- getContents
  case parseOnly parsePlay lines of
         Left error -> print error
         Right (players, maxMarble) -> print $ winningScore $ finishedGamestate maxMarble $ iterate oneMove (emptyPlayers players, emptyCircle)

