{-# LANGUAGE TemplateHaskell, Rank2Types, GADTs, FlexibleContexts, DeriveTraversable #-}
module Jugendstil.Doc where

import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import Graphics.Holz
import Graphics.Holz.Vertex (translate)
import Jugendstil.Color
import Linear
import qualified Data.BoundingBox as Box
import qualified Graphics.Holz.Text as Text

data Doc f a where
  Prim :: !a -> (Given Window => Box V2 Float -> IO [(Maybe Texture, PrimitiveMode, [Vertex])]) -> Doc f a
  Docs :: !a -> f (Doc f a) -> Doc f a
  Viewport :: !a -> Doc f a -> Doc f a
  deriving (Functor, Foldable, Traversable)

renderDoc :: (Foldable f, Given Window, MonadIO m) => (a -> Box V2 Float) -> Doc f a -> m ()
renderDoc k (Prim a mk) = liftIO $ mk (k a) >>= \xs -> forM_ xs $ \(tex, prim, vs) -> do
  buf <- registerVertex prim vs
  case tex of
    Nothing -> drawVertexPlain identity buf
    Just t -> drawVertex identity t buf
  releaseVertex buf
renderDoc k (Docs _ xs) = mapM_ (renderDoc k) xs
renderDoc k (Viewport a d) = do
  let box@(Box (V2 x0 y0) (V2 x1 y1)) = k a
  setViewport $ fmap round box
  liftIO $ setProjection $ ortho x0 x1 y1 y0 (-1) 1
  renderDoc k d
  setOrthographic

mouseOver :: (Foldable f, Given Window, MonadIO m, Monoid r)
  => Doc f (Box V2 Float, r) -> m r
mouseOver doc = getCursorPos <&> \pos -> foldMap
  (\(b, r) -> if Box.isInside pos b then r else mempty) doc

fill :: Monoid s => RGBA -> Doc f s
fill bg = Prim mempty $ \(Box (V2 x0 y0) (V2 x1 y1)) -> pure [(Nothing, TriangleStrip,
  [ Vertex (V3 x0 y0 0) (V2 0 0) (V3 0 0 1) bg
  , Vertex (V3 x1 y0 0) (V2 1 0) (V3 0 0 1) bg
  , Vertex (V3 x0 y1 0) (V2 0 1) (V3 0 0 1) bg
  , Vertex (V3 x1 y1 0) (V2 1 1) (V3 0 0 1) bg
  ])]

annotation :: Lens' (Doc f s) s
annotation f (Prim s bg) = f s <&> \s' -> Prim s' bg
annotation f (Docs s ws) = f s <&> \s' -> Docs s' ws
annotation f (Viewport s d) = f s <&> \s' -> Viewport s' d

text :: Monoid a => Text.Renderer -> V4 Float -> String -> Doc f a
text renderer fg str = Prim mempty $ \(Box (V2 x0 y0) (V2 x1 y1)) -> do
  renderer `Text.runRenderer` do
    let size = (y1 - y0) * 2 / 3
    Text.string size fg str
    V2 x y <- Text.getOffset
    let k = min 1 $ (x1 - x0) / x
    Text.render $ translate (V3 (x1 - 4 - k * x) (y0 + (y1 - y0) * 0.75 - k * y) 1)
      !*! scaled (V4 k k k 1)
    Text.clear
  return []
