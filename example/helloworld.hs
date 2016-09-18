{-# LANGUAGE LambdaCase, Rank2Types, ScopedTypeVariables, FlexibleContexts, GADTs #-}

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
import Control.Monad.Trans
import Control.Monad.Trans.Iter
import Data.Monoid

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
  let whiteText = text writer (pure 1) :: Monoid a => String -> Document a
  win <- openWindow Resizable (Box (V2 0 0) (V2 640 480))
  retract $ withWindow win $ iterDocument $ \lang -> return $ Right $ rowsDL $ do
    Just 0.25 ==> pure "English" <$ whiteText "Hello, world"
    Just 0.25 ==> pure "Deutsch" <$ whiteText "Hallo, Welt"
    Just 0.25 ==> pure "日本語" <$ whiteText "こんにちは世界"
    Nothing ==> whiteText (maybe "" id $ getFirst lang)
