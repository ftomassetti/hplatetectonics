module Plate where

import Graphics hiding (Point)
import qualified Data.Map.Strict as M
import qualified HeightMap.Base as HB
import Basic

data Segment = Segment
               deriving Show

data Plate = Plate { plateId :: PlateId, plateAge :: Int, plateVelocity :: Int, plateMomentum :: Int, plateSegments :: [Segment] }
             deriving (Show)


-- Structure used just when building the plates
data PlateBuilder = PlateBuilder { plateExplorableBorders :: [Point] }

-----------------------------------------------------------
-- Plate
-----------------------------------------------------------

createPlate id = Plate id 0 0 0 []

createPlateBuilder p = PlateBuilder [p]

resetSegments plate = plate

movePlate plate = plate

type PlateId = Int
type OwnerMap = M.Map Point PlateId
type PlatesMap = M.Map PlateId Plate
type PlateBuildersMap = M.Map PlateId PlateBuilder


-- Given it is a toroidal world, the left border is on the point most on the left,
-- unless the plate wraps aroung the world border. If it does the most left point
-- is actually
--plateLeft :: OwnerMap -> PlateId -> Int
--plateLeft ownerMap plateId = minimum . map pointX $ getPoints ownerMap plateId

--plateRight :: OwnerMap -> PlateId -> Int
--plateRight ownerMap plateId = maximum . map pointX $ getPoints ownerMap plateId


getPoints :: OwnerMap -> PlateId -> [Point]
getPoints ownerMap plateId = map fst . filter (\(point,id) -> id==plateId) $ M.assocs ownerMap
