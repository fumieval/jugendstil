{-# LANGUAGE TemplateHaskell, GADTs, FlexibleContexts, DeriveTraversable #-}
module Jugendstil.Doc where

import Control.Lens
import Linear
import Graphics.Holz
import Data.Maybe (fromMaybe)
import qualified Data.BoundingBox as Box
import qualified Graphics.Holz.Text as Text
import Graphics.Holz.Vertex
import Jugendstil.Color
import Control.Object
import Debug.Trace

data Doc a where
  Prim :: !a -> (Box V2 Float -> [(Maybe Texture, PrimitiveMode, [Vertex])]) -> Doc a
  Docs :: !a -> [Doc a] -> Doc a
  Viewport :: !a -> Doc a -> Doc a
  deriving (Functor, Foldable, Traversable)

renderDoc :: Given Window => (a -> Box V2 Float) -> Doc a -> IO ()
renderDoc k (Prim a mk) = draw identity $ mk (k a)
renderDoc k (Docs _ xs) = mapM_ (renderDoc k) xs
renderDoc k (Viewport a b) = do
    let Box (V2 x0 y0) (V2 x1 y1) = k a
    glViewport x0 y0 x1 y1
    setProjection $ ortho x0 x1 y1 y0 (-1) 1
    renderDoc k b

mouseOver :: Given Window => (a -> Box V2 Float) -> Doc a -> IO [a]
mouseOver k doc = getCursorPos <&> \pos ->
    foldMap (\(a -> if Box.isInside pos (k box) then [a] else []) doc

fill :: Monoid s => RGBA -> Doc s
fill bg = Prim mempty $ \(Box (V2 x0 y0) (V2 x1 y1)) -> (TriangleStrip,
  [ Vertex (V3 x0 y0 0) (V2 0 0) (V3 0 0 1) bg
  , Vertex (V3 x1 y0 0) (V2 1 0) (V3 0 0 1) bg
  , Vertex (V3 x0 y1 0) (V2 0 1) (V3 0 0 1) bg
  , Vertex (V3 x1 y1 0) (V2 1 1) (V3 0 0 1) bg
  ])

instance Monoid a => Monoid (Doc a) where
  mempty = Docs mempty []
  mappend a b = Docs mempty [a, b]

style :: Lens' (Doc s) s
style f (Prim s bg) = f s <&> \s' -> Prim s' bg
style f (Docs s ws) = f s <&> \s' -> Docs s' ws
style f (Viewport s d) = f s <&> \s' -> Viewport s' d
