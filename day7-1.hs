{-# LANGUAGE OverloadedStrings #-}

module Main where
import Prelude hiding (getContents, foldr, map, filter, null, take)

import Data.Attoparsec.Text hiding (take)
import Text.Pretty.Simple (pPrint)
import Data.Text.IO
import Data.Set hiding (foldr)
import Data.List(sort, groupBy)
import Data.Foldable(foldr)
import Data.Function(on)

newtype ID = ID Char deriving (Eq, Ord)

instance Show ID where
  show (ID a) = [a]

data Requirement = Req { rId :: ID
                       , rDependsOn :: [ID]
                       } deriving (Show, Eq)

instance Ord Requirement where
  compare (Req id1 _) (Req id2 _ ) = compare id1 id2

parseReq:: Parser Requirement
parseReq = do
  string "Step "
  dependsOn <- anyChar
  string " must be finished before step "
  id <- anyChar
  string " can begin."
  endOfLine
  return $ Req (ID id) [(ID dependsOn)]

parseReqs:: Parser [Requirement]
parseReqs = many1 parseReq

allIds:: Set Requirement -> Set ID
allIds = foldr (union . getDeps) empty where
  getDeps (Req id rDependsOn) = fromList (id:rDependsOn)

startTasks:: Set Requirement -> Set ID
startTasks r = foldr removeDeps (allIds r) r where
  removeDeps (Req id dependsOn) s =  s \\ (singleton id)

mergeDeps:: [Requirement] -> Set Requirement
mergeDeps reqs = foldr collectDeps empty $ groupBy ((==) `on` rId) $ sort reqs where
  collectDeps l@((Req id deps):rs) s = insert (Req id (sort $ concatMap rDependsOn l)) s

satisfiedReqs:: Set Requirement -> Set ID -> Set Requirement
satisfiedReqs reqs sats = filter reqsMet reqs where
  reqsMet (Req id reqs) = fromList reqs `isSubsetOf` sats
  

topoSort:: Set Requirement -> [ID]
topoSort reqs = go reqs startTs (toList startTs)  where
  startTs = take 1 $ startTasks reqs
  go reqs satisfied order | null reqs = order
                          | otherwise = let newSatisfied = take 1 $ satisfiedReqs reqs satisfied
                                            satisfiedIds = map rId newSatisfied in
                                          go (reqs \\ newSatisfied) (satisfied `union` satisfiedIds) (order ++ toAscList satisfiedIds)

addEmptyReqs:: Set Requirement -> Set Requirement
addEmptyReqs s = union emptyTasks s where
  emptyTasks = map (\id -> Req id []) (startTasks s)

main:: IO ()
main = do
  lines <- getContents
  case parseOnly parseReqs lines of
    Left error -> print error
    Right reqs -> do
      print $ fmap (\(ID a) -> a) $ topoSort $ addEmptyReqs $ mergeDeps reqs
