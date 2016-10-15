{-# LANGUAGE NoMonomorphismRestriction
           , PartialTypeSignatures
           , FlexibleContexts #-}
-- | Draw my floor plan
module Lib
  (
    -- * Domain types
    Room(..)
    -- * Rendering functions
  , render
  ) where

import Diagrams.Prelude (Diagram, circle)
import Diagrams.Backend.Canvas (B)

data Room = Room Int Int

render :: Room -> Diagram B
render _ = circle 1
