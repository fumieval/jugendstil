{-# LANGUAGE TemplateHaskell, LambdaCase, DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
module Jugendstil.Doc.Layout
  ( Layout(..)
  , computeStyle
  , Document
  , rows
  , columns
  , docs
  , DoList
  , unDoList
  , (==>)
  , rowsDL
  , columnsDL)
  where

import Data.BoundingBox as Box
import Data.Monoid
import Jugendstil.Doc
import Linear

data Layout a = Horizontal [(Maybe Float, a)]
    | Vertical [(Maybe Float, a)]
    | Stack [a]
    deriving (Functor, Foldable, Traversable)

computeStyle :: Box V2 Float -> Doc Layout a -> Doc [] (Box V2 Float, a)
computeStyle box (Prim a bg) = Prim (box, a) bg
computeStyle box@(Box (V2 x0 y0) (V2 x1 y1)) (Docs a (Horizontal xs))
  = Docs (box, a) $ boxes x0 $ sortLayout (x1 - x0) xs
  where
    boxes x ((w, d):ws) = computeStyle (Box (V2 x y0) (V2 (x + w) y1)) d : boxes (x + w) ws
    boxes _ [] = []
computeStyle box@(Box (V2 x0 y0) (V2 x1 y1)) (Docs a (Vertical xs))
  = Docs (box, a) $ boxes y0 $ sortLayout (y1 - y0) xs
  where
    boxes y ((h, d):hs) = computeStyle (Box (V2 x0 y) (V2 x1 (y + h))) d : boxes (y + h) hs
    boxes _ [] = []
computeStyle box (Docs a (Stack xs)) = Docs (box, a) (map (computeStyle box) xs)

sortLayout :: Float -> [(Maybe Float, a)] -> [(Float, a)]
sortLayout total xs0 = let (r, ys) = go total 0 xs0 in map ($ r) ys where
  go :: Float -> Int -> [(Maybe Float, a)] -> (Float, [Float -> (Float, a)])
  go t n ((Just r, a) : xs) = let v = total * r
      in (const (v, a) :) <$> go (t - v) n xs
  go t n ((Nothing, a) : xs) = (:) (\r -> (r, a)) <$> go t (n + 1) xs
  go t n _ = (t / fromIntegral n, [])

type Document = Doc Layout

rows :: Monoid a => [(Maybe Float, Document a)] -> Document a
rows xs = Docs mempty $ Vertical xs

columns :: Monoid a => [(Maybe Float, Document a)] -> Document a
columns xs = Docs mempty $ Horizontal xs

docs :: Monoid a => [Document a] -> Document a
docs xs = Docs mempty $ Stack xs

type DoList a = (,) (Endo [a])

unDoList :: DoList a x -> [a]
unDoList (Endo f, _) = f []

(==>) :: a -> b -> DoList (a, b) ()
a ==> b = (Endo ((a, b):), ())

infix 0 ==>

rowsDL :: Monoid a => DoList (Maybe Float, Document a) x -> Document a
rowsDL = rows . unDoList

columnsDL :: Monoid a => DoList (Maybe Float, Document a) x -> Document a
columnsDL = columns . unDoList
