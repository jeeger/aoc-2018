{-# LANGUAGE OverloadedStrings #-}
module Main where
import Prelude hiding (putStr,getContents)

import Data.Text.IO
import Data.Functor(($>))
import Text.Pretty.Simple(pPrint)
import Control.Applicative((<|>))
import Control.Monad.Trans.State
import Control.Monad.Trans.Class
import Data.List(partition)
import Data.Monoid((<>))

import qualified Data.HashMap.Strict as M
import qualified Data.Set as S
import qualified Formatting as F
import qualified Data.Attoparsec.Text as P
import qualified Data.Text as T


import Debug.Trace(traceShowId)

data Turning = TLeft | TStraight | TRight deriving (Show, Eq)
data Direction = DLeft | DUp | DRight | DDown deriving (Show, Eq)

data CartState = CState { pos:: (Int, Int)
                        , direction:: Direction
                        , turning:: Turning } deriving (Show)
                          
data LabyrinthElement = LCurveLeft (Int, Int)
                      | LCurveRight (Int, Int)
                      | LCart CartState
                      | LIntersection (Int, Int) deriving (Show)

newtype Cart = Cart CartState deriving (Show)

data MapElem = Curve Turning
             | Intersection
             | Road deriving (Show)

newtype Map = Map (M.HashMap (Int, Int) MapElem) deriving (Show)
newtype Carts = Carts (S.Set Cart) deriving (Show)

instance Ord Cart where
  compare (Cart (CState (x1, y1) _ _)) (Cart (CState (x2, y2) _ _)) = x1 `compare` x2 <> y1 `compare` y2

instance Eq Cart where
  (==) (Cart (CState (x1, y1) _ _)) (Cart (CState (x2, y2) _ _)) = x1 == x2 && y1 == y2

emptyCart:: (Int, Int) -> Direction -> CartState
emptyCart pos dir = CState pos dir TLeft 

parseCurveElem:: (Int, Int) -> P.Parser LabyrinthElement
parseCurveElem pos = ("/" $> LCurveRight pos) <|> ("\\" $> LCurveLeft pos) 

parseCartElem:: (Int, Int) -> P.Parser LabyrinthElement
parseCartElem pos = (">" $> LCart (emptyCart pos DRight))
                    <|> ("<" $> LCart (emptyCart pos DLeft))
                    <|> ("^" $> LCart (emptyCart pos DUp))
                    <|> ("v" $> LCart (emptyCart pos DDown))


parseCurve:: StateT (Int,Int) P.Parser LabyrinthElement
parseCurve = do
  (x, y) <- get
  elt <- lift (parseCurveElem (x, y))
  put (x+1, y)
  return elt

parseCart:: StateT (Int, Int) P.Parser LabyrinthElement
parseCart = do
  (x, y) <- get
  elt <- lift (parseCartElem (x, y))
  put (x + 1, y)
  return elt

parseIntersection:: StateT (Int, Int) P.Parser LabyrinthElement
parseIntersection = do
  (x, y) <- get
  elt <- lift ("+" $> LIntersection (x, y))
  put (x+1, y)
  return elt

parseUninteresting:: StateT (Int, Int) P.Parser ()
parseUninteresting = do
  (x, y) <- get
  elt <- lift (P.satisfy (P.inClass "- |"))
  put (x + 1, y)
  return ()

parseNewline:: StateT (Int, Int) P.Parser ()
parseNewline = do
  (x, y) <- get
  lift P.endOfLine
  put (0, y + 1)
  return ()

parseNextElement:: StateT (Int, Int) P.Parser LabyrinthElement
parseNextElement = do
  P.skipMany parseUninteresting
  element <- parseIntersection <|> parseCart <|> parseCurve
  P.skipMany parseUninteresting
  P.option () parseNewline
  return element

parseAll:: StateT (Int, Int) P.Parser [LabyrinthElement]
parseAll = P.many1 parseNextElement

isCart:: LabyrinthElement -> Bool
isCart LCart {} = True
isCart _ = False

getCarts:: [LabyrinthElement] -> ([LabyrinthElement], [LabyrinthElement])
getCarts = partition isCart

toCartSet:: [LabyrinthElement] -> Carts
toCartSet el = Carts $ foldr (\c s -> S.insert (toCart c) s) S.empty el where
  toCart (LCart cs) = Cart cs
  toCart c = error $ "Not a cart: " ++ show c

toCoordinateMap:: [LabyrinthElement] -> Map
toCoordinateMap l = Map $ foldr (\e m -> M.insert (pos e) (toM e) m) M.empty l where
  pos (LIntersection p) = p
  pos (LCurveLeft p) = p
  pos (LCurveRight p) = p
  pos c@LCart{} = error $ "Not a map element: " ++ show c
  toM LIntersection{} = Intersection
  toM (LCurveLeft _) = Curve TLeft
  toM (LCurveRight _) = Curve TRight
  toM c@LCart{} = error $ "Not a map element: " ++ show c

turnDirection:: Turning -> Direction -> Direction
turnDirection TLeft dir = nextDirection dir
turnDirection TRight dir = prevDirection dir
turnDirection TStraight dir = dir

nextDirection:: Direction -> Direction
nextDirection DLeft = DUp
nextDirection DUp = DRight
nextDirection DRight = DDown
nextDirection DDown = DLeft

prevDirection:: Direction -> Direction
prevDirection dir = iterate nextDirection dir !! 3

nextTurn:: Turning -> Turning
nextTurn TLeft = TStraight
nextTurn TStraight = TRight
nextTurn TRight = TLeft

stepSingleCart:: Map -> Cart -> Cart
stepSingleCart (Map m) = modifyCart . stepCart where
  stepCart (Cart cs@(CState (x, y) DLeft _)) = Cart cs{pos = (x-1, y)}
  stepCart (Cart cs@(CState (x, y) DUp _)) = Cart cs{pos = (x, y-1)}
  stepCart (Cart cs@(CState (x, y) DRight _)) = Cart cs{pos = (x+1, y)}
  stepCart (Cart cs@(CState (x, y) DDown _)) = Cart cs{pos = (x, y+1)}
  modifyCart (Cart cs@(CState pos dir turning)) = case M.lookupDefault Road pos m of
                                                  Intersection -> Cart cs{direction=turnDirection turning dir, turning=nextTurn turning}
                                                  Curve t -> Cart cs{direction=turnDirection t dir}
                                                  Road -> Cart cs
step:: Map -> Carts -> Either (Int, Int) Carts
step m (Carts s) = go s S.empty where
  go s r | S.null s = Right (Carts r)
  go s r = let (nextcart@(Cart cs)) = stepSingleCart m (S.elemAt 0 s)
               othercarts = S.deleteAt 0 s in
             if nextcart `S.member` othercarts || nextcart `S.member` r then
               Left (pos cs)
             else
               go othercarts (S.insert nextcart r)
               
main:: IO ()
main = do
  lines <- getContents
  case P.parseOnly (evalStateT parseAll (0,0)) lines of
    Left err -> print err
    Right elems -> do
      let (cartsE, mapE) = getCarts elems
          (mp, carts) = (toCoordinateMap mapE, toCartSet cartsE)
      print $ (step mp) $ step mp carts
