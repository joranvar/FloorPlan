{-# LANGUAGE NoMonomorphismRestriction
           , PartialTypeSignatures
           , FlexibleContexts #-}
-- | Draw my floor plan
module Lib
  (
    -- * Domain types
    Room(..)
  , Door(..)
  , DoorDirection(..)
    -- * Rendering functions
  , floorPlan
  ) where

import Data.List (foldl')
import Diagrams.Prelude
import Diagrams.Backend.Canvas (B)

data Room = Room { dimensions::V2 Double
                 , doors::[ Door ] }

data Door = Door { wall::Direction V2 Double
                 , dir::DoorDirection }

data DoorDirection = InwardLeft | InwardRight | OutwardLeft | OutwardRight

renderRoom :: Room -> Diagram B
renderRoom r =
  (uncurry rect . unr2 $ dimensions r)
  `atop`
  (foldl' atop mempty $ map (\d -> renderDoor (dimensions r * (fromDirection $ wall d)) d) $ doors r)

renderDoor :: V2 Double -> Door -> Diagram B
renderDoor w (Door d t) =
  let t' = case t of
             InwardLeft -> negate
             OutwardRight -> id
             InwardRight -> id
             OutwardLeft -> negate
      d' = case t of
             InwardLeft -> fmap negate d
             InwardRight -> fmap negate d
             _ -> d
  in (arc' 90 d' (t' 1/4 @@ turn) # lw thin
      `atop`
      (arrowV (90 * fromDirection d')) # lw thin)
     # rectEnvelope (mkP2 0 0) (mkR2 0 0)
     # translate (w / 2) -- To wall
     # translate (-45 * (rotate (t' 1/4 @@ turn) (fromDirection d'))) -- Align center

mkRoom :: Double -> Double -> Room
mkRoom x y = Room (mkR2 x y) []

bedroom1, bedroom2, kitchen, bathroom, hall, livingroom, toilet ::  Room
bedroom1   = mkRoom (120 + 100 + 15)                     (60 + 100 + 125 + 120)
           # addDoor (Door (direction unit_Y) InwardRight)
bedroom2   = mkRoom (55 + 110 + 50)                      (60 + 100 + 125 + 120)
           # addDoor (Door (direction unitX) InwardLeft)
kitchen    = mkRoom (60 + 60 + 100 + 15 + 55 + 110 + 50) (100 + 20 + 100)
           # addDoor (Door (direction unitX) InwardRight)
bathroom   = mkRoom (75 + 100 + 75)                      (125 + 120)
           # addDoor (Door (direction unit_Y) OutwardLeft)
hall       = mkRoom (75 + 100 + 75)                      (100 + 60 + 100)
           # addDoor (Door (direction unit_Y) InwardRight)
livingroom = mkRoom (148 + 122 + 140)                    (122 + 200 + 125 + 162 + 142)
           # addDoor (Door (direction unit_X) InwardLeft)
toilet     = mkRoom 100                                  100
           # addDoor (Door (direction unitY) OutwardRight)

addDoor :: Door -> Room -> Room
addDoor d r = r { doors = d:doors r }

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
