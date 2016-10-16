{-# LANGUAGE NoMonomorphismRestriction
           , PartialTypeSignatures
           , FlexibleContexts
           , RecordWildCards #-}
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
import Diagrams.Backend.SVG (B)

data Room = Room { dimensions::V2 Double
                 , doors::[ Door ]
                 , furniture::[ Furniture ]}

data Door = Door { wall::Direction V2 Double
                 , dir::DoorDirection
                 , fromCorner::Double }

data DoorDirection = InwardLeft | InwardRight | OutwardLeft | OutwardRight

data Furniture = Furniture { loc::V2 Double
                           , dim::V2 Double
                           , name::String }

renderRoom :: Room -> Diagram B
renderRoom r =
  (uncurry rect . unr2 $ dimensions r)
  `atop`
  (foldl' atop mempty $ map (\d -> renderDoor r d) $ doors r)
  `atop`
  (foldl' atop mempty $ map (\f -> renderFurniture r f) $ furniture r)

renderDoor :: Room -> Door -> Diagram B
renderDoor r (Door d t o) =
  let t' = case t of
             InwardLeft -> negate
             OutwardRight -> id
             InwardRight -> id
             OutwardLeft -> negate
      d' = case t of
             InwardLeft -> fmap negate d
             InwardRight -> fmap negate d
             _ -> d
      wOffset = (/ 2) $ dimensions r * (fromDirection $ d)
      centeringDirection = rotate (t' 1/4 @@ turn) (fromDirection d')
      cOffset = -45 * centeringDirection
      wallDirection = rotate (1/4 @@ turn) (fromDirection d)
      wSize = abs . sum $ dimensions r * wallDirection
      dOffset = if o >= 0 then pure (o + 50 - (wSize/2)) * wallDirection
                         else pure (wSize + o - 50 - (wSize/2)) * wallDirection
  in (arc' 90 d' (t' 1/4 @@ turn) # lw thin
      `atop`
      (arrowV (90 * fromDirection d')) # lw thin)
     # rectEnvelope (mkP2 0 0) (mkR2 0 0) -- Size 0 envelope
     # translate wOffset -- To wall
     # translate cOffset -- Correct for center of door
     # translate dOffset -- Place correctly in wall

renderFurniture :: Room -> Furniture -> Diagram B
renderFurniture r Furniture{..} =
  ((uncurry rect) (unr2 dim) # lw thin # lc blue <> baselineText name)
  # translate (loc - fmap (/ 2) (dimensions r - dim))
  # rectEnvelope (mkP2 0 0) (mkR2 0 0) -- Size 0 envelope

mkRoom :: Double -> Double -> Room
mkRoom x y = Room (mkR2 x y) [] []

bedroom1, bedroom2, kitchen, bathroom, hall, livingroom, toilet ::  Room
bedroom1   = mkRoom (120 + 100 + 15)                     (60 + 100 + 125 + 120)
           # addDoor (Door (direction unit_Y) InwardRight (-15))
bedroom2   = mkRoom (55 + 110 + 50)                      (60 + 100 + 125 + 120)
           # addDoor (Door (direction unitX) InwardLeft 60)
           # addFurniture (Furniture (mkR2 0 0) (mkR2 (55 + 110 + 50) 60) "closet")
kitchen    = mkRoom (60 + 60 + 100 + 15 + 55 + 110 + 50) (100 + 20 + 100)
           # addDoor (Door (direction unitX) InwardRight 120)
           # addFurniture (Furniture (mkR2 0 0) (mkR2 60 (100 + 20 + 100)) "counter")
bathroom   = mkRoom (75 + 100 + 75)                      (125 + 120)
           # addDoor (Door (direction unit_Y) OutwardLeft 75)
hall       = mkRoom (75 + 100 + 75)                      (100 + 60 + 100)
           # addDoor (Door (direction unit_Y) InwardRight (-130))
livingroom = mkRoom (148 + 122 + 140)                    (122 + 200 + 125 + 162 + 142)
           # addDoor (Door (direction unit_X) InwardLeft (122 + 125))
toilet     = mkRoom 100                                  100
           # addDoor (Door (direction unitY) OutwardRight 0)

addDoor :: Door -> Room -> Room
addDoor d r = r { doors = d:doors r }

addFurniture :: Furniture -> Room -> Room
addFurniture f r = r { furniture = f:furniture r }

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
