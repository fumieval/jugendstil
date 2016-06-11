{-# LANGUAGE LambdaCase, GADTs #-}

import Control.Monad
import Control.Object
import Graphics.Holz (withHolz)
import Graphics.Holz.Text (typewriter)
import Jugendstil
import Linear

main = withHolz $ do
  writer <- typewriter "example/Oxygen-Regular.ttf"
  let whiteText = text writer (pure 1) :: String -> Doc Arrangement
  (_, draw) <- openWidget (Box (V2 0 0) (V2 640 48)) $ liftO $ \case
    Draw -> return
      $ harray [whiteText "Hello,", whiteText "World"]
    MouseButton _ -> return ()
    MouseCursor _ -> return ()
    MouseScroll _ -> return ()
    Keyboard _ -> return ()
  forever draw
