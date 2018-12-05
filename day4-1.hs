{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module Main where

import Prelude hiding (getContents)
import Data.Attoparsec.Text
import Data.Text.IO
import Data.HashMap.Strict
import GHC.Generics
import Data.Hashable
import Data.List(sort)
import Data.Monoid((<>))


newtype GuardId = GuardId {
  guardId:: Int
  } deriving (Show, Generic, Eq)

instance Hashable GuardId

data GuardAction = ShiftBegin GuardId
                 | FallAsleep
                 | WakeUp
                 deriving (Show, Eq, Generic)

instance Hashable GuardAction

newtype Minute = Minute {
  minute:: Int
  } deriving (Show, Eq, Ord)

data Date = Date {datedYear :: Int
                 , datedMonth :: Int
                 , datedDay :: Int
                 } deriving (Show, Eq, Ord)

emptyTime = Minute (-1)

data DatedTimedGuardAction = DAction Date Minute GuardAction deriving (Show, Eq)

instance Ord DatedTimedGuardAction where
  compare (DAction d1 m1 _) (DAction d2 m2 _) = (compare d1 d2) <> (compare m1 m2)

data TimedGuardAction = Action Minute GuardAction deriving (Show)

parseShiftBegin:: Parser GuardAction
parseShiftBegin = do
  string "Guard #"
  id <- decimal
  string " begins shift"
  return $ ShiftBegin $ GuardId id
  <?> "parseShiftBegin"

parseAsleep:: Parser GuardAction
parseAsleep = do
  string "falls asleep"
  return FallAsleep
  <?> "parseAsleep"

parseWakeUp:: Parser GuardAction
parseWakeUp = do
  string "wakes up"
  return WakeUp
  <?> "parseWakeUp"


parseAction:: Parser GuardAction
parseAction = choice [parseShiftBegin, parseAsleep, parseWakeUp] <?> "parseAction"

parseTimestamp:: Parser (Date, Minute)
parseTimestamp = do
  char '['
  year <- decimal
  char '-'
  month <- decimal
  char '-'
  day <- decimal
  space
  decimal
  char ':'
  minute <- decimal
  char ']'
  return $ (Date year month day, Minute minute)
  <?> "parseTimestamp"

parseLine:: Parser DatedTimedGuardAction
parseLine = do
  (date, minute) <- parseTimestamp
  space
  action <- parseAction
  endOfLine
  return $ DAction date minute action
  <?> "parseLine"

parser:: Parser [DatedTimedGuardAction]
parser =  many1 parseLine

diffmin:: Minute -> Minute -> Int
diffmin (Minute m1) (Minute m2) | m1 > m2 = error "Minute 1 > minute 2"
                                | m1 > 60 || m2 > 60 = error "Minute > 60"
                                | otherwise = m2 - m1

dropDate:: [DatedTimedGuardAction] -> [TimedGuardAction]
dropDate [] = []
dropDate ((DAction _ m a): as) = (Action m a):dropDate as

sleepingMinutes:: [TimedGuardAction] -> HashMap GuardId Int
sleepingMinutes actions = go actions (GuardId 0) empty emptyTime where
  go [] _ m _ = m
  go ((Action ts ac):acs) curguard m sleeptime =
    case ac of
      ShiftBegin g -> go acs g m emptyTime
      FallAsleep -> go acs curguard m ts
      WakeUp -> go acs curguard (insertWith (+) curguard (diffmin sleeptime ts) m) emptyTime

argmax:: Ord a => (k, a) -> HashMap k a -> k
argmax init m = fst $ foldrWithKey argmax' init m where
  argmax' k v (mk, mv) = if v > mv then
                           (k, v)
                         else
                           (mk, mv)

insertKeys:: (Eq k, Hashable k) => [k] -> (v -> v -> v) -> v -> HashMap k v -> HashMap k v
insertKeys keys update value m = Prelude.foldr (\k m -> insertWith update k value m) m keys
  
sleepiestGuard:: HashMap GuardId Int -> GuardId
sleepiestGuard g = argmax (GuardId 0, 0) g

sleepiestMinute:: [TimedGuardAction] -> Int
sleepiestMinute actions = go actions empty (GuardId 0) emptyTime where
  sleeper = sleepiestGuard $ sleepingMinutes actions
  go [] m _ _ = guardId sleeper * argmax (0, 0) m
  go ((Action ts ac):acs) m curguard sleepTime =
    case ac of
      ShiftBegin g -> go acs m g emptyTime
      FallAsleep -> go acs m curguard ts
      WakeUp -> if curguard == sleeper then
                  go acs (insertKeys (sleepMinutes sleepTime ts) (+) 1 m) curguard emptyTime
                else
                  go acs m curguard emptyTime
  sleepMinutes (Minute ts1) (Minute ts2) = [ts1..ts2-1]

main:: IO ()
main = do
  lines <- getContents
  case parseOnly parser lines of
    Left error -> print error
    Right actions -> do
      let sortedActions = dropDate $ sort actions
      print $ sleepiestMinute sortedActions
