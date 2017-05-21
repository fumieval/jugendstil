{-# LANGUAGE TemplateHaskell, LambdaCase, FlexibleContexts, DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
module Jugendstil.Doc.Layout
  ( Layout(..)
  , matchLayout
  , computeStyle
  , Document
  , renderDocument
  , rows
  , columns
  , docs
  , margin
  -- * DoList
  , DoList
  , unDoList
  , (==>)
  , rowsDL
  , columnsDL)
  where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Iter
import Data.BoundingBox
import Data.BoundingBox as Box
import Data.Monoid
import Graphics.Holz
import Jugendstil.Doc
import Linear

data Layout a = Horizontal [(Maybe Float, a)]
    | Vertical [(Maybe Float, a)]
    | Stack [a]
    | Extend (Box V2 Float -> Box V2 Float) a
    deriving (Functor, Foldable, Traversable)

matchLayout :: Applicative m => (Maybe a -> b -> m c) -> Layout a -> Layout b -> m (Layout c)
matchLayout f (Horizontal xs) (Horizontal ys) = Horizontal
  <$> zipWithM (\m (s, b) -> (,) s <$> f (snd <$> m) b)
    (map Just xs ++ repeat Nothing) ys
matchLayout f (Vertical xs) (Vertical ys) = Vertical
  <$> zipWithM (\m (s, b) -> (,) s <$> f (snd <$> m) b)
    (map Just xs ++ repeat Nothing) ys
matchLayout f (Stack xs) (Stack ys) = Stack <$> zipWithM f (map Just xs ++ repeat Nothing) ys
matchLayout f (Extend _ a) (Extend g b) = Extend g <$> f (Just a) b
matchLayout f _ l = traverse (f Nothing) l

renderDocument :: Document a -> ShaderT (HolzT IO) (Doc Layout (Box V2 Float, a))
renderDocument doc = do
  box <- getBoundingBox
  let doc' = computeStyle box doc
  renderDoc fst doc'
  return doc'

computeStyle :: Box V2 Float -> Doc Layout a -> Doc Layout (Box V2 Float, a)
computeStyle box (Prim a bg) = Prim (box, a) bg
computeStyle box@(Box (V2 x0 y0) (V2 x1 y1)) (Docs a (Horizontal xs))
  = Docs (box, a) $ Horizontal $ zip (map fst xs) $ boxes x0 $ sortLayout (x1 - x0) xs
  where
    boxes x ((w, d):ws) = computeStyle (Box (V2 x y0) (V2 (x + w) y1)) d : boxes (x + w) ws
    boxes _ [] = []
computeStyle box@(Box (V2 x0 y0) (V2 x1 y1)) (Docs a (Vertical xs))
  = Docs (box, a) $ Vertical $ zip (map fst xs) $ boxes y0 $ sortLayout (y1 - y0) xs
  where
    boxes y ((h, d):hs) = computeStyle (Box (V2 x0 y) (V2 x1 (y + h))) d : boxes (y + h) hs
    boxes _ [] = []
computeStyle box (Docs a (Stack xs)) = Docs (box, a) $ Stack $ map (computeStyle box) xs
computeStyle box (Docs a (Extend f d)) = Docs (box, a) $ Extend f $ computeStyle (f box) d
computeStyle box (Viewport a d) = Viewport (box, a) (computeStyle box d)

sortLayout :: Float -> [(Maybe Float, a)] -> [(Float, a)]
sortLayout total xs0 = let (r, ys) = go total 0 xs0 in map ($ r) ys where
  go :: Float -> Int -> [(Maybe Float, a)] -> (Float, [Float -> (Float, a)])
  go t n ((Just r, a) : xs) = let v = total * r
      in (const (v, a) :) <$> go (t - v) n xs
  go t n ((Nothing, a) : xs) = (:) (\r -> (r, a)) <$> go t (n + 1) xs
  go t n _ = (t / fromIntegral n, [])

type Document = Doc Layout

rows :: Monoid a => [(Maybe Float, Document a)] -> Document a
rows xs = Docs mempty $ Vertical xs

columns :: Monoid a => [(Maybe Float, Document a)] -> Document a
columns xs = Docs mempty $ Horizontal xs

docs :: Monoid a => [Document a] -> Document a
docs xs = Docs mempty $ Stack xs

margin :: Monoid a => V4 Float -> Document a -> Document a
margin (V4 t r b l) = Docs mempty
  . Extend (\(V2 x0 y0 `Box` V2 x1 y1)
      -> V2 (x0 + l) (y0 + t) `Box` V2 (x1 - r) (y1 - b))

type DoList a = (,) (Endo [a])

unDoList :: DoList a x -> [a]
unDoList (Endo f, _) = f []

(==>) :: a -> b -> DoList (a, b) ()
a ==> b = (Endo ((a, b):), ())

infix 0 ==>

rowsDL :: Monoid a => DoList (Maybe Float, Document a) x -> Document a
rowsDL = rows . unDoList

columnsDL :: Monoid a => DoList (Maybe Float, Document a) x -> Document a
columnsDL = columns . unDoList
