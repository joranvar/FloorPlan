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

import Diagrams.Prelude (Diagram, rect, (|||), alignL, (#), vcat, alignT, atop, alignBR)
import Diagrams.Backend.Canvas (B)

data Room = Room Int Int

render :: Room -> Diagram B
render (Room w h) = rect (fromIntegral w) (fromIntegral h)

bedroom1, bedroom2, kitchen, bathroom, hall, livingroom, toilet :: Room
bedroom1 = Room (120 + 100 + 15) (60 + 100 + 125 + 120)
bedroom2 = Room (55 + 110 + 50) (60 + 100 + 125 + 120)
kitchen = Room (60 + 60 + 100 + 15 + 55 + 110 + 50) (100 + 20 + 100)
bathroom = Room (75 + 100 + 75) (125 + 120)
hall = Room (75 + 100 + 75) (100 + 60 + 100)
livingroom = Room (148 + 122 + 140) (122 + 200 + 125 + 162 + 142)
toilet = Room 100 100

floorPlan :: Diagram B
floorPlan =
  vcat [ ((render bedroom1) ||| (render bedroom2)) # alignL
       , (render kitchen # alignBR `atop` render toilet # alignBR) # alignL
       ] # alignT
  |||
  vcat [ render bathroom
       , render hall
       ] # alignT
  |||
  vcat [ render livingroom
       ] # alignT
