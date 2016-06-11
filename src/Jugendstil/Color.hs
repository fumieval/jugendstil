module Jugendstil.Color where
import Linear

type RGB = V3 Float

type RGBA = V4 Float

lerp' :: Float -> Float -> Float -> Float
lerp' a b t = a + (b - a) * t

hsv :: Float -> Float -> Float -> RGB
hsv h_ s v = pure (v - c) + rgb h_ where
  c = s * v
  rgb h
    | h < 0 = rgb (h + 360)
    | h < 60 = V3 c x 0
    | h < 120 = V3 x c 0
    | h < 180 = V3 0 c x
    | h < 240 = V3 0 x c
    | h < 300 = V3 x 0 c
    | h < 360 = V3 c 0 x
    | otherwise = rgb (h - 360)
  x = c * (1 - abs (h_ / 60 - fromIntegral (floor (h_ / 120) :: Int) * 2 - 1))

-- Light blue to purple
intensityGradient :: Float -> Float -> Float -> RGB
intensityGradient h h' x = hsv
  (lerp' h h' x)
  (0.4 + x * 0.2)
  0.7

opaque :: Num a => V3 a -> V4 a
opaque (V3 r g b) = V4 r g b 1

colorQty :: Float -- curve
  -> Int -- unit size
  -> Int -- quantity
  -> RGBA
colorQty k u q = opaque $ intensityGradient 180 300
  $ 1 - exp (negate $ (**k) $ fromIntegral q / fromIntegral u)

vegetableGradient :: Float -> RGB
vegetableGradient = genericGradient 12 0 120 0.1 0.7 0.5

genericGradient :: Float -- hue bump
  -> Float -- min hue
  -> Float -- max hue
  -> Float -- base saturation
  -> Float -- min brightness
  -> Float -- max brightness
  -> Float
  -> RGB
genericGradient hb h0 h1 s0 v0 v1 x = hsv
  (lerp' h0 h1 hq)
  (s0 + (1 - s0) * abs x ** 0.3)
  (lerp' v0 v1 vq)
  where
    r = hb / (h1 - h0) / 2
    hq = signum x * r + (0.5 - r) * x + 0.5
    vq = 1 / (1 + exp (-8 * x))
