{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module Main where

import Prelude hiding (getContents)
import Data.Text.IO
import Data.Attoparsec.Text hiding (take)
import Data.Function(fix)


import Data.Char(isUpper, toUpper, toLower)
import Text.Pretty.Simple (pPrint)

data Polarity = Up
              | Down deriving (Show, Eq)
              
data Unit = Unit { uPolarity :: Polarity
                 , uType :: Char
                 } deriving (Eq)

instance Show Unit where
  show (Unit p t) = if p == Up then
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
  return $ Unit (if isUpper unit then Up else Down) (toLower unit)

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
  
main:: IO ()
main = do
  lines <- getContents
  case parseOnly parsePolymer lines of
    Left error -> print error
    Right polymer -> print $ length . punit $ reactAll polymer
