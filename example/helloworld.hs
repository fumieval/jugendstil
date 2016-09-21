{-# LANGUAGE LambdaCase, Rank2Types, ScopedTypeVariables, FlexibleContexts, GADTs #-}

import Graphics.Holz
import qualified Graphics.Holz.Text as Text
import Jugendstil.Doc.TextBox
import Jugendstil
import Linear
import Data.Monoid
import Control.Monad.IO.Class
import Control.Monad.Trans.Iter

mainDoc :: Given Window => Text.Renderer -> Doc [] (Box V2 Float, First String) -> TextBox -> IterT IO ()
mainDoc writer doc_ tb = do
  let whiteText = text writer (pure 1) :: Monoid a => String -> Document a
  lang <- mouseOver id doc_
  tb' <- updateTextBox tb
  doc <- renderDocument $ rowsDL $ do
    Just 0.25 ==> (\() -> pure "English") <$> docTextBox writer (pure 1) tb'
    Just 0.25 ==> pure "Deutsch" <$ whiteText "Hallo, Welt"
    Just 0.25 ==> pure "日本語" <$ whiteText "こんにちは世界"
    Nothing ==> whiteText (maybe "" id $ getFirst lang)
  delay $ mainDoc writer doc tb'

main = withHolz $ do
  writer <- Text.typewriter "example/font/TakaoPGothic.ttf"
  win <- openWindow Resizable (Box (V2 0 0) (V2 640 480))
  retract $ iterWithWindow win $ do
    setTitle "Hello, world"
    mainDoc writer (Docs (pure 0, mempty) []) ("Hello, world", 0)
