{-# LANGUAGE LambdaCase, FlexibleContexts #-}
module Jugendstil.Doc.Widget where
import Control.Lens
import Jugendstil.Doc
import Jugendstil.Doc.Layout
import Linear
import Data.BoundingBox
import Graphics.Holz
import qualified Graphics.Holz.Text as Text
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Iter

newtype Widget m a = Widget
    { unWidget :: Document (a, Document (Box V2 Float) -> m (Widget m a)) }

iterWidget :: (Given Window, MonadIO m) => Widget m a -> IterT m b
iterWidget w0 = renderDocument (unWidget w0) >>= delay . go where
  go d = do
    w <- lift $ view (annotation . _2 . _2) d (fmap fst d)
    lift (renderDocument (unWidget w)) >>= delay . go

liftWidget :: Applicative m => Document a -> Widget m a
liftWidget doc = Widget $ fmap (\a -> (a, const $ pure $ liftWidget doc)) doc

layoutWidget :: (Applicative m, Monoid a) => Layout (Widget m a) -> Widget m a
layoutWidget ws = Widget $ Docs (mempty, \case
  Docs box layout -> layoutWidget
      <$> matchLayout (\d' w -> maybe (pure w) (snd $ view annotation $ unWidget w) d') layout ws
  -- Layout mismatch!?
  _ -> pure $ layoutWidget ws)
  $ fmap unWidget ws

rowsW :: (Applicative m, Monoid a) => [(Maybe Float, Widget m a)] -> Widget m a
rowsW = layoutWidget . Vertical

columnsW :: (Applicative m, Monoid a) => [(Maybe Float, Widget m a)] -> Widget m a
columnsW = layoutWidget . Horizontal
