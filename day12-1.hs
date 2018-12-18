{-# LANGUAGE OverloadedStrings #-}
module Main where
import Prelude hiding (putStr,getContents)

import Data.Attoparsec.Text(Parser, decimal, char, space, endOfLine, many1, parseOnly, anyChar, manyTill, (<?>))
import Data.Text
import Data.Text.IO
import Data.Functor(($>))
import Control.Applicative((<|>))
import qualified Formatting as F


data PlantState = Plant
                | NoPlant deriving Show

data Rule = Rule [PlantState] PlantState deriving Show
newtype Rules = Rules [Rule] deriving Show
newtype Plants = Plants [PlantState] deriving Show

parsePlantState:: Parser PlantState
parsePlantState =
  ("#" $> Plant <|> "." $> NoPlant)
  <?> "parsePlantState"

parseRule:: Parser Rule
parseRule = do
  input <- manyTill parsePlantState space
  "=> "
  result <- parsePlantState
  endOfLine
  return $ Rule input result
  <?> "parseRule"

parseInitialState:: Parser Plants
parseInitialState = do
  "initial state: "
  plants <- manyTill parsePlantState endOfLine
  return $ Plants  plants
  <?> "parseInitialState"

parseAll:: Parser (Rules, Plants)
parseAll = do
  plants <- parseInitialState
  endOfLine
  rules <- many1 parseRule
  return (Rules rules, plants)
  <?> "parseAll"

main:: IO ()
main = do
  lines <- getContents
  case parseOnly parseAll lines of
    Left err -> print err
    Right (r, p) -> do
      print r
      print p
    
