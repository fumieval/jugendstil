{-# LANGUAGE TemplateHaskell, GADTs, FlexibleContexts, DeriveTraversable #-}
module Jugendstil.Doc where

import Control.Lens
import Linear
import Graphics.Holz
import Data.Maybe (fromMaybe)
import qualified Data.BoundingBox as Box
import qualified Graphics.Holz.Text as Text
import Graphics.Holz.Vertex
import Jugendstil.Color
import Control.Object
import Debug.Trace

data Doc a where
  Text :: !a -> Text.Renderer -> RGBA -> String -> Doc a
  Prim :: !a -> (Box V2 Float -> (PrimitiveMode, [Vertex])) -> Doc a
  HArray :: !a -> [Doc a] -> Doc a
  VArray :: !a -> [Doc a] -> Doc a
  Layers :: !a -> [Doc a] -> Doc a
  deriving (Functor, Foldable, Traversable)

text :: (Monoid s) => Text.Renderer -> RGBA -> String -> Doc s
text = Text mempty

fill :: Monoid s => RGBA -> Doc s
fill bg = Prim mempty $ \(Box (V2 x0 y0) (V2 x1 y1)) -> (TriangleStrip,
  [ Vertex (V3 x0 y0 0) (V2 0 0) (V3 0 0 1) bg
  , Vertex (V3 x1 y0 0) (V2 1 0) (V3 0 0 1) bg
  , Vertex (V3 x0 y1 0) (V2 0 1) (V3 0 0 1) bg
  , Vertex (V3 x1 y1 0) (V2 1 1) (V3 0 0 1) bg
  ])

circle :: Monoid s => RGBA -> Doc s
circle bg = Prim mempty $ \box@(Box (V2 x0 y0) (V2 x1 y1)) -> (TriangleFan,
  [ Vertex (V3 x y 0) (V2 0 0) (V3 0 0 1) bg
  | t <- [0, 2 * pi / 20..2 * pi]
  , let r = min (x1 - x0) (y1 - y0) / 2
  , let x = (x0 + x1) / 2 - cos t * r
  , let y = (y0 + y1) / 2 + sin t * r
  ])

harray :: (Monoid s) => [Doc s] -> Doc s
harray = HArray mempty

varray :: (Monoid s) => [Doc s] -> Doc s
varray = VArray mempty

instance Monoid a => Monoid (Doc a) where
  mempty = Layers mempty []
  mappend a b = Layers mempty [a, b]

getStyle :: Monoid s => Doc s -> s
getStyle (Text s _ _ _) = s
getStyle (Prim s _) = s
getStyle (HArray s ws) = s `mappend` foldMap getStyle ws
getStyle (VArray s ws) = s `mappend` foldMap getStyle ws
getStyle (Layers s _) = s

style :: Traversal' (Doc s) s
style f (Text s w fg str) = f s <&> \s' -> Text s' w fg str
style f (Prim s bg) = f s <&> \s' -> Prim s' bg
style f (HArray s ws) = f s <&> \s' -> HArray s' ws
style f (VArray s ws) = f s <&> \s' -> VArray s' ws
style f (Layers s ws) = f s <&> \s' -> Layers s' ws

renderDoc :: Given Window => Doc (Box V2 Float, a) -> IO ()
renderDoc (Text (Box (V2 _ y0) (V2 x1 y1), _) typewriter fg str) = do
  typewriter ..- Text.simpleR ((y1 - y0) * 2 / 3) fg str
    (translate (V3 (x1 - 4) (y0 + (y1 - y0) * 0.75) 1))
renderDoc (Prim (box@(Box v0 v1), _) mk) = draw identity $ mk box
renderDoc (HArray _ ws) = mapM_ renderDoc ws
renderDoc (VArray _ ws) = mapM_ renderDoc ws
renderDoc (Layers _ ws) = mapM_ renderDoc ws

mouseOver :: (Given Window, Monoid r) => (a -> r) -> Doc (Box V2 Float, a) -> IO r
mouseOver k doc = getCursorPos <&> \pos ->
    foldMap (\(box, a) -> if Box.isInside pos box then k a else mempty) doc

data Position = Absolute Float | Relative Float | Auto deriving Show

data Arrangement a = Arrangement
  { _width :: !Position
  , _height :: !Position
  , _xpos :: !Position
  , _ypos :: !Position
  , _arranged :: a
  } deriving (Functor, Foldable, Traversable)
makeLenses ''Arrangement

fillAuto :: Box V2 Float -> Arrangement a -> (Box V2 Float, a)
fillAuto box@(Box (V2 x0 y0) (V2 x1 y1)) (Arrangement w h x y a) = (box
  & Box.size (pure 0.5) %~ (\v -> k <$> v <*> V2 w h)
  & Box.position (pure 0.5) %~ \(V2 cx cy) -> V2 (pos cx x0 x1 x) (pos cy y0 y1 y)
  , a)
  where
    k t (Relative v) = t * v
    k _ (Absolute t) = t
    k t Auto = t

    pos _ t0 t1 (Relative v) = t0 * (1 - v) + t1 * v
    pos t0 _ _ (Absolute t) = t0 + t
    pos t0 _ _ Auto = t0

instance Monoid a => Monoid (Arrangement a) where
  mempty = Arrangement Auto Auto Auto Auto mempty
  mappend (Arrangement w0 h0 x0 y0 a) (Arrangement w1 h1 x1 y1 b) = Arrangement
    (k w0 w1)
    (k h0 h1)
    (k x0 x1)
    (k y0 y1)
    (mappend a b)
    where
      k Auto x = x
      k x Auto = x
      k (Relative x) (Relative y) = Relative $ max x y
      k (Absolute x) (Absolute y) = Absolute $ max x y
      k _ x = x

computeStyle :: Box V2 Float -> Doc (Arrangement a) -> Doc (Box V2 Float, a)
computeStyle s0 (Text s w fg str) = Text (fillAuto s0 s) w fg str
computeStyle s0 (Prim s bg) = Prim (fillAuto s0 s) bg
computeStyle s0 (HArray s xs0) = HArray s' $ go y0 (map (fromMaybe defH) ws0) xs0
  where
    go x (w : ws) (t : ts) = computeStyle (Box (V2 x y0) (V2 (x + w) y1)) t
        : go (x + w) ws ts
    go _ _ _ = []
    s'@(Box (V2 x0 y0) (V2 x1 y1), _) = fillAuto s0 s
    free = x1 - x0 - sumOf (folded . folded) ws0
    defH = free / fromIntegral (lengthOf (folded . only Nothing) ws0)
    ws0 = do
      -- Arrangement p _ _ _ <- map (view style) xs0
      _ <- xs0
      let p = Auto
      return $ case p of
        Absolute w -> Just w
        Relative k -> Just $ (x1 - x0) * k
        Auto -> Nothing
computeStyle s0 (VArray s xs0) = VArray s' $ go y0 (map (fromMaybe defH) hs0) xs0
  where
    go y (h : hs) (t : ts) = computeStyle (Box (V2 x0 y) (V2 x1 (y + h))) t
        : go (y + h) hs ts
    go _ _ _ = []
    s'@(Box (V2 x0 y0) (V2 x1 y1), _) = fillAuto s0 s
    free = y1 - y0 - sumOf (folded . folded) hs0
    defH = free / fromIntegral (lengthOf (folded . only Nothing) hs0)
    hs0 = do
        -- Arrangement _ p _ _ <- map (view style) xs0
      _ <- xs0
      let p = Auto
      return $ case p of
        Absolute h -> Just h
        Relative k -> Just $ (y1 - y0) * k
        Auto -> Nothing
computeStyle s0 (Layers s xs) = let s' = fillAuto s0 s in Layers s' $ map (computeStyle $ fst s') xs
