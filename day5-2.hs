{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module Main where

import Prelude hiding (getContents)
import Data.Text.IO
import Data.Attoparsec.Text hiding (take)
import Data.Function(fix)


import Data.Char(isUpper, toUpper, toLower)
import Text.Pretty.Simple (pPrint)

import Data.Set hiding (foldr, filter)

data Polarity = Up
              | Down deriving (Show, Eq)

newtype UnitType = UType Char deriving (Show, Eq, Ord)
              
data Unit = Unit { uPolarity :: Polarity
                 , uType :: UnitType
                 } deriving (Eq)

instance Show Unit where
  show (Unit p (UType t)) = if p == Up then
                      [toUpper t]
                    else
                      [t]
    

reacts:: Unit -> Unit -> Bool
reacts u1 u2 | uType u1 /= uType u2 = False
             | uPolarity u1 /= uPolarity u2 = True
             | otherwise = False

newtype Polymer = Polymer { punit :: [Unit] } deriving (Show, Eq)

parseUnit:: Parser Unit
parseUnit = do
  unit <- letter
  return $ Unit (if isUpper unit then Up else Down) (UType (toLower unit))

parsePolymer:: Parser Polymer
parsePolymer = do
  units <- many1 parseUnit
  endOfLine
  return $ Polymer units

react:: Polymer -> Polymer
react p@(Polymer []) = p
react (Polymer l) = Polymer $ foldr unitReact [] l where
  unitReact p [] = [p]
  unitReact p (r:rs) = if p `reacts` r then
                                rs
                              else
                                (p:(r:rs))

reactAll:: Polymer -> Polymer
reactAll p = if (react p) == p then p else reactAll (react p)

printPolymer:: Polymer -> IO ()
printPolymer (Polymer p) = do
  print $ concat [show u | u <- p]

units:: Polymer -> Set UnitType
units (Polymer l) = foldr collectUnits empty l where
  collectUnits (Unit p t) s = insert t s


shortestReacted:: Polymer -> Set UnitType -> Int
shortestReacted p s = foldr (min . (lengthRemoved p)) (length $ punit p) s

lengthRemoved:: Polymer -> UnitType -> Int
lengthRemoved (Polymer l) t = length $ punit $ reactAll $ Polymer $ filter (notSame t) l where
  notSame t1 (Unit _ t2) = t1 /= t2
  
main:: IO ()
main = do
  lines <- getContents
  case parseOnly parsePolymer lines of
    Left error -> print error
    Right polymer -> print $ shortestReacted polymer (units polymer)
