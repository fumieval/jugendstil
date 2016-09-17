{-# LANGUAGE LambdaCase, GADTs #-}

import Control.Monad
import Control.Object
import Graphics.Holz
import Graphics.Holz.Vertex
import qualified Graphics.Holz.Text as Text
import Jugendstil
import Linear
import Data.Function
import Debug.Trace
import Control.Monad


text :: Monoid a => Text.Renderer -> V4 Float -> String -> Document a
text renderer fg str = Prim mempty $ \(Box (V2 x0 y0) (V2 x1 y1)) -> do
  renderer ..- do
    let size = (y1 - y0) * 2 / 3
    Text.string size fg str
    V2 x y <- Text.getOffset
    let k = min 1 $ (x1 - x0) / x
    Text.render $ translate (V3 (x1 - 4 - k * x) (y0 + (y1 - y0) * 0.75 - k * y) 1)
      !*! scaled (V4 k k k 1)
    Text.clear
  return []

main = withHolz $ do
  writer <- Text.typewriter "example/Oxygen-Regular.ttf"
  let whiteText = text writer (pure 1) :: String -> Document ()
  win <- openWindow Resizable (Box (V2 0 0) (V2 640 480))
  fix $ \self -> do
      b <- withFrame win $ do
          box@(Box (V2 x0 y0) (V2 x1 y1)) <- getBoundingBox
          setProjection $ ortho x0 x1 y1 y0 (-1) 1
          renderDoc fst $ computeStyle box $ rows $ liste $ do
            Just 0.3 ==> whiteText "Hello, world"
            Just 0.3 ==> whiteText "Hallo, Welt"
            Nothing ==> whiteText "こんにちは世界"
          windowShouldClose
      unless b self
