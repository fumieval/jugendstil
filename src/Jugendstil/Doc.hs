{-# LANGUAGE TemplateHaskell, GADTs, FlexibleContexts, DeriveTraversable #-}
module Jugendstil.Doc where

import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import Control.Object
import Data.Maybe (fromMaybe)
import Graphics.Holz
import Graphics.Holz.Vertex
import Jugendstil.Color
import Linear
import qualified Data.BoundingBox as Box
import qualified Graphics.Holz.Text as Text

data Doc f a where
  Prim :: !a -> (Box V2 Float -> IO [(Maybe Texture, PrimitiveMode, [Vertex])]) -> Doc f a
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
renderDoc k (Viewport a b) = do
  let Box (V2 x0 y0) (V2 x1 y1) = k a
  -- glViewport x0 y0 x1 y1
  liftIO $ setProjection $ ortho x0 x1 y1 y0 (-1) 1
  renderDoc k b

mouseOver :: (Foldable f, Given Window) => (a -> Box V2 Float) -> Doc f a -> IO [a]
mouseOver k doc = getCursorPos <&> \pos ->
  foldMap (\a -> [a | Box.isInside pos (k a)]) doc

fill :: Monoid s => RGBA -> Doc f s
fill bg = Prim mempty $ \(Box (V2 x0 y0) (V2 x1 y1)) -> pure [(Nothing, TriangleStrip,
  [ Vertex (V3 x0 y0 0) (V2 0 0) (V3 0 0 1) bg
  , Vertex (V3 x1 y0 0) (V2 1 0) (V3 0 0 1) bg
  , Vertex (V3 x0 y1 0) (V2 0 1) (V3 0 0 1) bg
  , Vertex (V3 x1 y1 0) (V2 1 1) (V3 0 0 1) bg
  ])]

style :: Lens' (Doc f s) s
style f (Prim s bg) = f s <&> \s' -> Prim s' bg
style f (Docs s ws) = f s <&> \s' -> Docs s' ws
style f (Viewport s d) = f s <&> \s' -> Viewport s' d
