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

import Diagrams.Prelude (Diagram, rect, (|||), alignL, (#), vcat)
import Diagrams.Backend.Canvas (B)

data Room = Room Int Int

render :: Room -> Diagram B
render (Room w h) = rect (fromIntegral w) (fromIntegral h)

bedroom1, bedroom2, kitchen :: Room
bedroom1 = Room (120 + 100 + 15) (60 + 100 + 125 + 120)
bedroom2 = Room (55 + 110 + 50) (60 + 100 + 125 + 120)
kitchen = Room (60 + 60 + 100 + 15 + 55 + 110 + 50) (100 + 20 + 100)

floorPlan :: Diagram B
floorPlan =
  vcat
  [ ((render bedroom1) ||| (render bedroom2)) # alignL
  , (render kitchen) # alignL
  ]
