{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}

module Main where
import Prelude hiding (getContents, foldr, filter, null, take, drop)
import qualified Prelude as P

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
import Text.Pretty.Simple (pPrint)
import Control.Applicative ((<|>))

newtype Metadata = Meta { mData:: [Int] } deriving (Show, Eq)
data Tree = Node { tChildren:: [Tree]
                 , tMetadata:: Metadata } deriving (Show, Eq)


parseChildNode:: Parser Tree
parseChildNode = do
  space
  parseNode
    <?> "parseChildNode"

parseMetadata:: Parser Int
parseMetadata = do
  space
  md <- decimal
  return md
  <?> "parseMetadata"

parseNode:: Parser Tree
parseNode = do
  childCount <- decimal
  space
  metadataCount <- decimal
  children <- count childCount parseChildNode
  metadata <- count metadataCount parseMetadata
  return $ Node children (Meta metadata)
  <?> "parseNode"

treeSum:: Tree -> Int
treeSum (Node [] (Meta m)) = sum m
treeSum (Node children (Meta m)) = (sum m) + (sum $ map treeSum children)

main:: IO ()
main = do
  lines <- getContents
  case parseOnly parseNode lines of
    Left error -> print error
    Right t -> print $ treeSum t
