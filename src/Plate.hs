module Plate where

import Graphics
import qualified Data.Map.Strict as M

data Segment = Segment
               deriving Show

data Plate = Plate { plateVelocity :: Int, plateMomentum :: Int, segments :: [Segment] }
             deriving (Show)


-- Structure used just when building the plates
data PlateBuilder = PlateBuilder { plateExplorableBorders :: [Point] }

-----------------------------------------------------------
-- Plate
-----------------------------------------------------------

createPlate = Plate 0 0 []

createPlateBuilder p = PlateBuilder [p]

resetSegments plate = plate

movePlate plate = plate

type PlateId = Int



type OwnerMap = M.Map Point PlateId
type PlatesMap = M.Map PlateId Plate
type PlateBuildersMap = M.Map PlateId PlateBuilder


