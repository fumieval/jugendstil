module Jugendstil.Doc.Layout where

data Position = Absolute Float | Relative Float | Auto | Min Position Position | Max Position Position deriving Show

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

data Layout = Stack | Horizontal | Vertical

computeStyle :: Box V2 Float -> Doc (Arrangement a) -> Doc (Box V2 Float, a)
computeStyle box (Text s w fg str) = Text (fillAuto box s) w fg str
computeStyle box (Prim s bg) = Prim (fillAuto box s) bg
computeStyle box (Array lo s xs) = Array lo s' $ zipWith computeStyle boxes xs
  where
    boxes = case lo of
      Stack -> box <$ xs
      Horizontal -> arrange box (map (view style) xs)
    s' = fillAuto box s
