{-# LANGUAGE GADTs, FlexibleContexts #-}

module Jugendstil.Widget where

import Jugendstil.Doc
import Control.Object
import Graphics.Holz
import Linear
import Data.Reflection (give)
import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Concurrent

data Widget x where
  MouseButton :: Chatter Int -> Widget ()
  MouseCursor :: V2 Float -> Widget ()
  MouseScroll :: V2 Float -> Widget ()
  Keyboard :: Chatter Key -> Widget ()
  Draw :: Widget (Doc (Arrangement ()))

openWidget :: (MonadMask m, MonadIO m) => Box V2 Float -> Object Widget m -> IO (Window, m ())
openWidget box0 obj = openWindow Resizable box0 >>= \w -> give w $ do
  ref <- new obj
  evt <- newMVar (return ())
  linkMouseButton $ \b -> modifyMVar_ evt $ return . (>>(ref.-MouseButton b))
  linkMouseCursor $ \v -> modifyMVar_ evt $ return . (>>(ref.-MouseCursor v))
  linkMouseScroll $ \v -> modifyMVar_ evt $ return . (>>(ref.-MouseScroll v))
  linkKeyboard $ \k -> modifyMVar_ evt $ return . (>>(ref.-Keyboard k))

  return $ (,) w $ withFrame w $ do
    m <- liftIO $ swapMVar evt (return ())
    m
    box@(Box (V2 x0 y0) (V2 x1 y1)) <- getBoundingBox
    setProjection $ ortho x0 x1 y1 y0 (-1) 1

    doc <- ref .- Draw
    let doc' = computeStyle box doc
    liftIO $ renderDoc doc'
