{-# LANGUAGE LambdaCase, FlexibleContexts #-}
module Jugendstil
  ( module Jugendstil.Doc
  , module Jugendstil.Doc.Layout
  , module Jugendstil.Color
  , Box(..)
  ) where

import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Iter
import Data.BoundingBox
import Graphics.Holz
import Jugendstil.Color
import Jugendstil.Doc
import Jugendstil.Doc.Layout
import Linear

-- | Iterate a function which yields a 'Document' from the previous arrangement,
-- rendering the result every step.
iterDocument :: (MonadIO m, Given Window, Monoid a)
  => (Doc [] (Box V2 Float, a) -> m (Either r (Document a)))
  -> IterT m r
iterDocument func = go $ Docs (Box zero zero, mempty) [] where
  go prevDoc = lift (func prevDoc) >>= \case
    Left r -> return r
    Right doc -> do
      box <- getBoundingBox
      let doc' = computeStyle box doc
      renderDoc fst doc'
      delay $ go doc'
