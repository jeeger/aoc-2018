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

parseMetadataEnd:: Parser Char
parseMetadataEnd = space <|> (endOfLine *> return '\n')

parseNode:: Parser Tree
parseNode = do
  childCount <- decimal
  space
  metadataCount <- decimal
  children <- count childCount parseChildNode
  metadata <- count metadataCount parseMetadata
  return $ Node children (Meta metadata)
  <?> "parseNode"

nodeValue:: Tree -> Int
nodeValue (Node [] (Meta metadata)) = sum metadata
nodeValue (Node children (Meta metadata)) = sum $ map nodeValue referencedChildren where
  validReferences = filter (\idx -> idx >= 1 && idx <= length children) metadata
  referencedChildren = zipWith (!!) (repeat children) (map (\ref -> ref - 1) validReferences)

main:: IO ()
main = do
  lines <- getContents
  case parseOnly parseNode lines of
    Left error -> print error
    Right t -> print $ nodeValue t
