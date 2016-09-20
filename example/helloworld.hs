{-# LANGUAGE LambdaCase, Rank2Types, ScopedTypeVariables, FlexibleContexts, GADTs #-}

import Graphics.Holz
import qualified Graphics.Holz.Text as Text
import Jugendstil
import Linear
import Data.Monoid

main = withHolz $ do
  writer <- Text.typewriter "example/font/TakaoPGothic.ttf"
  let whiteText = text writer (pure 1) :: Monoid a => String -> Document a
  win <- openWindow Resizable (Box (V2 0 0) (V2 640 480))
  retract $ iterWithWindow win $ iterDocument $ \doc -> do
    setTitle "Hello, world"
    lang <- mouseOver id doc
    return $ Right $ rowsDL $ do
      Just 0.25 ==> pure "English" <$ whiteText "Hello, world"
      Just 0.25 ==> pure "Deutsch" <$ whiteText "Hallo, Welt"
      Just 0.25 ==> pure "日本語" <$ whiteText "こんにちは世界"
      Nothing ==> whiteText (maybe "" id $ getFirst lang)
