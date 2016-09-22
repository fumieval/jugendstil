{-# LANGUAGE LambdaCase, Rank2Types, ScopedTypeVariables, FlexibleContexts, GADTs #-}

import Graphics.Holz
import qualified Graphics.Holz.Text as Text
import Jugendstil
import Linear
import Data.Monoid
import Control.Monad.Trans.Iter

mainDoc :: Given Window => Text.Renderer -> IterT IO ()
mainDoc writer = go $ Docs (pure 0, mempty) (Stack []) where
  whiteText = text writer (pure 1) :: Monoid a => String -> Document a
  go doc_ = do
    lang <- mouseOver doc_
    doc <- renderDocument $ rowsDL $ do
      Just 0.25 ==> pure "English" <$ whiteText "Hello, world!"
      Just 0.25 ==> pure "Deutsch" <$ whiteText "Hallo, Welt"
      Just 0.25 ==> pure "日本語" <$ whiteText "こんにちは世界"
      Nothing ==> whiteText (maybe "" id $ getFirst lang)
    delay $ go doc

main = withHolz $ do
  writer <- Text.typewriter "example/font/TakaoPGothic.ttf"
  win <- openWindow Resizable (Box (V2 0 0) (V2 640 480))
  retract $ iterWithWindow win $ do
    setTitle "Hello, world"
    mainDoc writer
