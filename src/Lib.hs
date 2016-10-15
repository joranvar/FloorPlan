{-# LANGUAGE NoMonomorphismRestriction
           , PartialTypeSignatures
           , FlexibleContexts #-}
-- | Draw my floor plan
module Lib
  (
    -- * Domain types
    Room(..)
    -- * Rendering functions
  , floorPlan
  ) where

import Control.Arrow ((***))
import Diagrams.Prelude (Diagram, rect, (|||))
import Diagrams.Backend.Canvas (B)

data Room = Room Int Int

render :: Room -> Diagram B
render (Room w h) = rect (fromIntegral w) (fromIntegral h)

bedroom1, bedroom2 :: Room
bedroom1 = Room (120 + 100 + 15) (60 + 100 + 125 + 120)
bedroom2 = Room (55 + 110 + 50) (60 + 100 + 125 + 120)

floorPlan :: Diagram B
floorPlan = uncurry (|||) . (render *** render) $ (bedroom1, bedroom2)
