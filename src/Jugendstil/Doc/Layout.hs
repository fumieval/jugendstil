{-# LANGUAGE TemplateHaskell, DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
module Jugendstil.Doc.Layout where

import Jugendstil.Doc
import Control.Lens
import Data.BoundingBox as Box
import Linear
import Data.Maybe (maybeToList)

data Layout a = Horizontal [(Float, a)]
    | Vertical [(Float, a)]
    | Stack [a]

computeStyle :: Box V2 Float -> Doc Layout a -> Doc [] (Box V2 Float)
computeStyle box (Prim _ bg) = Prim box bg
computeStyle box@(Box (V2 x0 y0) (V2 x1 y1)) (Docs _ (Horizontal xs))
  = Docs box $ boxes x0 xs
  where
    boxes x ((w, a):ws) = computeStyle (Box (V2 x y0) (V2 (x + w) y1)) a : boxes (x + w) ws
    boxes _ [] = []
computeStyle box@(Box (V2 x0 y0) (V2 x1 y1)) (Docs _ (Vertical xs))
  = Docs box $ boxes y0 xs
  where
    boxes y ((h, a):hs) = computeStyle (Box (V2 x0 y) (V2 x1 (y + h))) a : boxes (y + h) hs
    boxes _ [] = []
computeStyle box (Docs _ (Stack xs)) = Docs box (map (computeStyle box) xs)
