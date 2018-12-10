{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}

module Main where
import Prelude hiding (getContents, foldr, map, filter, null, take, drop)
import qualified Prelude as P

import Data.Attoparsec.Text hiding (take, takeWhile)
import Text.Pretty.Simple (pPrint)
import Data.Text.IO hiding (putStrLn)
import Data.Set hiding (foldr)
import Data.List(sort, groupBy)
import Data.Foldable(foldr)
import Data.Function(on)
import Data.Functor
import GHC.Generics(Generic)
import Data.Hashable
import Data.Char(ord)
import qualified Data.HashMap.Strict as M
import Text.Pretty.Simple (pPrint)

newtype TaskID = TID Char deriving (Eq, Ord)

instance Show TaskID where
  show (TID a) = [a]

data Requirement = Req { rId :: TaskID
                       , rDependsOn :: [TaskID]
                       } deriving (Show, Eq)

instance Ord Requirement where
  compare (Req id1 _) (Req id2 _ ) = compare id1 id2

newtype WorkerID = WID { wId :: Int } deriving (Show, Eq, Ord, Generic)

instance Hashable WorkerID

data WorkState = Working { wtID:: TaskID, wRemaining:: Int }
                 | Idle
                 | Finished {wtID:: TaskID } deriving (Show, Eq)

newtype WorkerState = WorkerState (M.HashMap WorkerID WorkState) deriving (Show)

parseReq:: Parser Requirement
parseReq = do
  string "Step "
  dependsOn <- anyChar
  string " must be finished before step "
  id <- anyChar
  string " can begin."
  endOfLine
  return $ Req (TID id) [TID dependsOn]

parseReqs:: Parser [Requirement]
parseReqs = many1 parseReq

allIds:: Set Requirement -> Set TaskID
allIds = foldr (union . getDeps) empty where
  getDeps (Req id rDependsOn) = fromList (id:rDependsOn)

startTasks:: Set Requirement -> Set TaskID
startTasks r = foldr removeDeps (allIds r) r where
  removeDeps (Req id dependsOn) s =  s \\ singleton id

mergeDeps:: [Requirement] -> Set Requirement
mergeDeps reqs = foldr collectDeps empty $ groupBy ((==) `on` rId) $ sort reqs where
  collectDeps l@(Req id deps:rs) = insert (Req id (sort $ concatMap rDependsOn l))

addEmptyReqs:: Set Requirement -> Set Requirement
addEmptyReqs s = emptyTasks `union` s where
  emptyTasks = map (\id -> Req id []) (startTasks s)

emptyWorkerState:: Int -> WorkerState
emptyWorkerState i = WorkerState $ M.fromList $ P.map (\i -> (WID i, Idle)) [1..i]

isFinished:: WorkState -> Bool
isFinished (Finished _) = True
isFinished _ = False

workLength:: TaskID -> Int
workLength (TID c) = ord c - 4

workLengthTest:: TaskID -> Int
workLengthTest (TID c) = ord c - 64

isAvailable:: WorkState -> Bool
isAvailable Idle = True
isAvailable _ = False

getFinished:: WorkerState -> (WorkerState, Set TaskID)
getFinished (WorkerState ws) = (WorkerState newState, fromList finished) where
  finished = P.map wtID $ M.elems $ M.filter isFinished ws
  newState = M.map (\case
                       Finished _ -> Idle
                       ws -> ws) ws

takeWork:: Set TaskID -> WorkerState -> (WorkerState, Set TaskID)
takeWork tids (WorkerState ws) | M.null $ M.filter isAvailable ws = (WorkerState ws, empty)
takeWork tids (WorkerState ws) = (WorkerState $ (M.fromList insertPairs) `M.union` ws, take (length insertPairs) tids) where
  free = M.filter isAvailable ws
  toInsert = take (M.size free) tids
  insertPairs = zipWith update (M.toList free) (toList toInsert)
  update (id, _) tid = (id, Working tid (workLength tid))
  

doWork:: WorkerState -> WorkerState
doWork (WorkerState ws) = WorkerState $ fmap workerWork ws where
  workerWork (Working id remaining) | remaining > 1 = (Working id (remaining - 1))
                                    | otherwise = Finished id
  workerWork w = w


satisfiedTasks:: Set TaskID -> Set Requirement -> Set TaskID
satisfiedTasks tids reqs = map rId $ filter allSatisfied reqs where
  allSatisfied (Req _ deps) = (fromList deps) `isSubsetOf` tids

filterReqs:: Set TaskID -> Set Requirement -> Set Requirement
filterReqs sid = filter notId where
  notId (Req id _) = id `notElem` sid

workStep:: (WorkerState, Set Requirement, Set TaskID) -> (WorkerState, Set Requirement, Set TaskID)
workStep (ws, reqs, finished) = let ws1 = doWork ws
                                    (ws2, alsoFinished) = getFinished ws1
                                    (ws3, assigned) = takeWork (satisfiedTasks (finished `union` alsoFinished) reqs) ws2 in
                                  (ws3, filterReqs assigned reqs, finished `union` alsoFinished)



workFinished:: (WorkerState, Set Requirement, Set TaskID) -> Bool
workFinished (WorkerState ws, reqs, _) | null reqs, all isAvailable ws = True
                                       | otherwise = False
  
main:: IO ()
main = do
  lines <- getContents
  case parseOnly parseReqs lines of
    Left error -> print error
    Right reqs -> do
      let reqS = addEmptyReqs $ mergeDeps reqs
      print $ (length $ takeWhile (not . workFinished) $ iterate workStep ((emptyWorkerState 5, reqS, empty))) - 1
      return ()
