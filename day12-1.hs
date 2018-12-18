{-# LANGUAGE OverloadedStrings #-}
module Main where
import Prelude hiding (putStr,getContents)

import Data.Attoparsec.Text(Parser, decimal, char, space, endOfLine, many1, parseOnly, anyChar, manyTill, (<?>))
import qualified Data.Text as T
import Data.Text.IO
import Data.Functor(($>))
import Text.Pretty.Simple(pPrint)
import Control.Applicative((<|>))
import qualified Data.HashMap.Strict as M
import qualified Formatting as F

import Debug.Trace(traceShowId)


data PlantState = Plant
                | NoPlant deriving (Show, Eq)

data Rule = Rule { rCond:: [PlantState]
                 , rResult:: PlantState
                 } deriving Show

newtype Rules = Rules [Rule] deriving Show
newtype Plants = Plants { pPlants:: M.HashMap Int PlantState} deriving Show



getPlantState:: Plants -> Int -> PlantState
getPlantState (Plants p) idx = M.lookupDefault NoPlant idx p

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
  return $ Plants $ M.fromList $ filter ((== Plant) . snd) $ zip [0..length plants] plants
  <?> "parseInitialState"

parseAll:: Parser (Rules, Plants)
parseAll = do
  plants <- parseInitialState
  endOfLine
  rules <- many1 parseRule
  return (Rules rules, plants)
  <?> "parseAll"

neighborhood:: Plants -> Int -> [PlantState]
neighborhood p key = map (getPlantState p) [key-2..key+2]

keysInRange:: Plants -> [Int]
keysInRange (Plants p) = [(mink-3)..(maxk+3)] where
  mink = minimum $ M.keys p
  maxk = maximum $ M.keys p

applyRule:: Rule -> Plants -> Plants
applyRule (Rule cond rresult) p = Plants $ applyRuleToKeys (keysInRange p) M.empty where
  applyRuleToKeys [] result = result
  applyRuleToKeys (k:ks) result = if neighborhood p k == cond && rresult == Plant then
                                    applyRuleToKeys ks (M.insert k Plant result)
                                  else
                                    applyRuleToKeys ks result

emptyPlant:: Plants
emptyPlant = Plants M.empty

plantUnion:: Plants -> Plants -> Plants
plantUnion (Plants p1) (Plants p2) = Plants $ M.unionWith (\_ _ -> error "Conflicting growths!") p1 p2

grow:: Rules -> Plants -> Plants
grow (Rules rs) p = foldr ruleUnion emptyPlant rs where
  ruleUnion r m = applyRule r p `plantUnion` m

value:: Plants -> Int
value (Plants p) = sum (M.keys p)

main:: IO ()
main = do
  lines <- getContents
  case parseOnly parseAll lines of
    Left err -> print err
    Right (r, p) ->
      print $ value $ iterate (grow r) p !! 20
