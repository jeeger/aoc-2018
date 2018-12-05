{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module Main where

import Prelude hiding (getContents)
import Data.Attoparsec.Text hiding (take)
import Data.Text.IO
import Text.Pretty.Simple (pPrint)
import Data.HashMap.Strict hiding (map)
import GHC.Generics
import Data.Hashable
import Data.List(sort)
import Data.Monoid((<>))


newtype GuardId = GuardId {
  guardId:: Int
  } deriving (Show, Generic, Eq, Ord)

instance Hashable GuardId

data GuardAction = ShiftBegin GuardId
                 | FallAsleep
                 | WakeUp
                 deriving (Show, Eq, Generic)

instance Hashable GuardAction

newtype Minute = Minute {
  minute:: Int
  } deriving (Show, Eq, Ord, Generic)

instance Hashable Minute

data Date = Date {datedYear :: Int
                 , datedMonth :: Int
                 , datedDay :: Int
                 } deriving (Show, Eq, Ord)

emptyTime = Minute (-1)

data DatedTimedGuardAction = DAction Date Minute GuardAction deriving (Show, Eq)

instance Ord DatedTimedGuardAction where
  compare (DAction d1 m1 _) (DAction d2 m2 _) = (compare d1 d2) <> (compare m1 m2)

data TimedGuardAction = Action Minute GuardAction deriving (Show)

newtype SleepRecord = Record { sleepRecord:: HashMap Minute Int } deriving (Show, Eq, Ord)
newtype GuardSleepRecord = GuardSleep {guardSleep:: HashMap GuardId SleepRecord } deriving (Show, Eq, Ord)

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

insertKeys:: (Eq k, Hashable k) => [k] -> (v -> v -> v) -> v -> HashMap k v -> HashMap k v
insertKeys keys update value m = Prelude.foldr (\k m -> insertWith update k value m) m keys

emptyRecord:: SleepRecord
emptyRecord = Record empty
  
insertSleep:: GuardId -> Minute -> Minute -> GuardSleepRecord -> GuardSleepRecord
insertSleep id (Minute m1) (Minute m2) (GuardSleep r) =
  let (Record sr) = lookupDefault emptyRecord id r in
    GuardSleep $ insert id (Record (insertKeys (map Minute [m1..m2-1]) (+) 1 sr)) r


addSleeps:: [TimedGuardAction] -> GuardSleepRecord
addSleeps actions = go actions (GuardSleep empty) (GuardId 0) (Minute 0) where
  go [] r _ _ = r
  go (Action m ac:acs) r g s = case ac of
    ShiftBegin id -> go acs r id (Minute 0)
    FallAsleep -> go acs r g m
    WakeUp -> go acs (insertSleep g s m r) g (Minute 0)

argmaxB:: Ord v => (k, v) -> HashMap k v -> (k,v)
argmaxB init m = foldrWithKey argmax' init m where
  argmax' k v (mk, mv) = if v > mv then
                           (k, v)
                         else
                           (mk, mv)

argmax init m = fst $ argmaxB init m

mostSleptMinute:: GuardSleepRecord -> (GuardId, Minute, Int)
mostSleptMinute (GuardSleep record) = foldrWithKey mostSleptMinute (GuardId 0, Minute 0, 0) record where
  mostSleptMinute k (Record v) (maxid, maxminute, maxslept) = let (minute, most) = argmaxB (Minute 0, 0) v in
    if most > maxslept then
      (k, minute, most)
    else
      (maxid, maxminute, maxslept)


solution:: GuardSleepRecord -> Int
solution r = let ((GuardId id), (Minute m), _) = mostSleptMinute r in
  id * m

main:: IO ()
main = do
  lines <- getContents
  case parseOnly parser lines of
    Left error -> print error
    Right actions -> do
      let sleeps = addSleeps $  dropDate $ sort actions
      pPrint $ solution sleeps
