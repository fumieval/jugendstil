{-# LANGUAGE TemplateHaskell, DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
module Jugendstil.Doc.Layout where

import Jugendstil.Doc
import Control.Lens
import Data.BoundingBox as Box
import Linear

data Layout a = Horizontal [a] | Vertical [a]

computeStyle :: Box V2 Float -> Doc Layout a -> Doc Layout (Box V2 Float)
computeStyle box (Prim _ bg) = Prim box bg
computeStyle box@(Box (V2 x0 y0) (V2 x1 y1)) (Docs _ (Horizontal xs))
  = Docs box $ Horizontal $ zipWith computeStyle (boxes x0 widths) xs
  where
    widths = solveLayout (x1 - x0) xs
    boxes x (w:ws) = Box (V2 x y0) (V2 (x + w) y1) : boxes (x + w) ws
    boxes _ [] = []
computeStyle box@(Box (V2 x0 y0) (V2 x1 y1)) (Docs _ (Vertical xs))
  = Docs box $ Vertical $ zipWith computeStyle (boxes y0 heights) xs
  where
    heights = solveLayout (y1 - y0) xs
    boxes y (h:hs) = Box (V2 x0 y) (V2 x1 (y + h)) : boxes (y + h) hs
    boxes _ [] = []

solveLayout :: Float -> [constraint] -> [Float]
solveLayout total constraints = replicate n (total / fromIntegral n) where
  n = length constraints
