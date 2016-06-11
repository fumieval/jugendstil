{-# LANGUAGE Rank2Types #-}
module Jugendstil.Widget.Chart where

import Linear
import Jugendstil.Doc
import Jugendstil.Color
import Control.Lens
import Graphics.Holz
import Control.Monad

plot :: RGBA -> [V2 Float] -> Doc Arrangement
plot col ps = Prim mempty $ \box@(Box (V2 x0 y0) (V2 x1 y1)) -> (LineStrip,
  [ Vertex (V3 x y 1) (V2 0 0) (V3 0 0 1) col
  | V2 u v <- ps
  , let x = x0 + u * (x1 - x0)
  , let y = y0 + v * (y1 - y0)
  ])

data MinMax a = NoMinMax | MinMax !a !a

instance Ord a => Monoid (MinMax a) where
  mempty = NoMinMax
  mappend NoMinMax a = a
  mappend a NoMinMax = a
  mappend (MinMax a b) (MinMax c d) = MinMax (min a c) (max b d)

getMinMaxOf :: Ord a => Fold s a -> s -> MinMax a
getMinMaxOf l = foldMapOf l (join MinMax)

rescale :: MinMax Float -> [V2 Float] -> [V2 Float]
rescale (MinMax y0 y1) = traverse . _y %~ \y -> (y1 - y) / (y1 - y0)
rescale NoMinMax = id
