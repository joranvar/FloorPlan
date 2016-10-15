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

import Diagrams.Prelude
import Diagrams.Backend.Canvas (B)

data Room = Room Int Int

renderRoom :: Room -> Diagram B
renderRoom (Room w h) = rect (fromIntegral w) (fromIntegral h)

bedroom1, bedroom2, kitchen, bathroom, hall, livingroom, toilet :: Room
bedroom1 = Room (120 + 100 + 15) (60 + 100 + 125 + 120)
bedroom2 = Room (55 + 110 + 50) (60 + 100 + 125 + 120)
kitchen = Room (60 + 60 + 100 + 15 + 55 + 110 + 50) (100 + 20 + 100)
bathroom = Room (75 + 100 + 75) (125 + 120)
hall = Room (75 + 100 + 75) (100 + 60 + 100)
livingroom = Room (148 + 122 + 140) (122 + 200 + 125 + 162 + 142)
toilet = Room 100 100

rooms :: Diagram B
rooms =
  vcat [ ((renderRoom bedroom1) ||| (renderRoom bedroom2)) # alignL
       , (renderRoom kitchen # alignBR `atop` renderRoom toilet # alignBR) # alignL
       ] # alignT
  |||
  vcat [ renderRoom bathroom
       , renderRoom hall
       ] # alignT
  |||
  vcat [ renderRoom livingroom
       ] # alignT

heightLines :: Double -> Double -> Diagram B
heightLines w _ =
  vcat [ strutY 122
       , (hrule w # lw ultraThin # dashingN [0.03,0.03] 0) <> baselineText "183"
       , strutY (200 + 125 + 162)
       , (hrule w # lw ultraThin # dashingN [0.03,0.03] 0) <> baselineText "183"
       ]

floorPlan :: Diagram B
floorPlan =
  heightLines (width rooms) (height rooms) # alignTL
  `atop`
  rooms # alignTL
