{-# LANGUAGE FlexibleContexts #-}
module Jugendstil.Doc.TextBox where

import Control.Lens
import Control.Monad.IO.Class
import Data.List (foldl')
import Graphics.Holz
import Graphics.Holz.Vertex
import Jugendstil.Doc
import Jugendstil.Doc.Widget
import Linear
import qualified Graphics.Holz.Text as Text

type TextBox = (String, Int)

updateTextBox :: (Given Window, MonadIO m) => TextBox -> m TextBox
updateTextBox (str, p) = do
  xs <- typedString
  ks <- typedKeys
  let V3 i j k = foldl' move (V3 0 0 p) ks
  let (l, r) = splitAt (p - i) str
  return (l ++ xs ++ drop (i + j) r, length xs + k - i - j)
  where
    len = length str
    move (V3 i j k) KeyBackspace = V3 (i + 1) j k
    move (V3 i j k) KeyDelete = V3 i (j + 1) k
    move (V3 i j k) KeyLeft = V3 i j (k - 1)
    move (V3 i j k) KeyRight = V3 i j (k + 1)
    move (V3 i j _) KeyHome = V3 i j 0
    move (V3 i j _) KeyEnd = V3 i j len
    move v _ = v

docTextBox :: Monoid a => Text.Renderer -> V4 Float -> TextBox -> Doc f a
docTextBox renderer fg (str, p) = Prim mempty $ \(Box (V2 x0 y0) (V2 x1 y1)) -> do
  renderer `Text.runRenderer` do
    let size = (y1 - y0) * 2 / 3
    let (l, r) = splitAt p str
    Text.string size fg l
    cursor <- Text.getOffset
    Text.string size fg r
    V2 x y <- Text.getOffset
    let k = min 1 $ (x1 - x0) / x
    let mat = translate (V3 (x1 - 4 - k * x) (y0 + (y1 - y0) * 0.75 - k * y) 1)
          !*! scaled (V4 k k k 1)
    draw (mat !*! (identity & translation . _xy .~ cursor))
      $ rectangle (V4 0.5 0.5 0.5 0.8) (V2 (-1) (-size)) (V2 1 0)
    Text.render mat
    Text.clear
  return []

textBoxWidget :: (MonadIO m, Given Window) => Text.Renderer -> V4 Float -> String -> Widget m String
textBoxWidget renderer fg str0 = go (str0, length str0) where
  go b = Widget $ (\() -> (fst b, \_ -> go <$> updateTextBox b)) <$> docTextBox renderer fg b
