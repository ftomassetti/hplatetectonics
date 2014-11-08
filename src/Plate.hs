module Plate where

import Graphics hiding (Point)
import qualified Data.Map.Strict as M
import qualified HeightMap.Base as HB
import Basic
import Data.Array.Repa
import qualified Data.List as L

data Segment = Segment
               deriving Show

data Plate = Plate {
                plateId :: PlateId,
                plateAge :: Int,
                plateVelocity :: Int,
                plateAngle    :: Float,
                plateMomentum :: Int,
                plateSegments :: [Segment] }
             deriving (Show)


-- Structure used just when building the plates
data PlateBuilder = PlateBuilder { plateExplorableBorders :: [Point] }

-----------------------------------------------------------
-- Plate
-----------------------------------------------------------

createPlate id angle = Plate id 0 1 angle 0 []

createPlateBuilder p = PlateBuilder [p]

resetSegments plate = plate

movePlate plate = plate

type PlateId = Int
type OwnerMap = M.Map Point PlateId
type PlatesMap = M.Map PlateId Plate
type PlateBuildersMap = M.Map PlateId PlateBuilder

plateMass :: OwnerMap -> ElevationMap -> PlateId -> Float
plateMass ownerMap elevMap plateId = foldr (+) 0 elevs
                                     where points = getPoints ownerMap plateId
                                           elevs  = L.map (\(Point x y) -> HB.getHeight $ elevMap ! (Z:.x:.y)) points

-- Given it is a toroidal world, the left border is on the point most on the left,
-- unless the plate wraps around the world border
plateLeft :: OwnerMap -> Int -> PlateId -> Int
plateLeft ownerMap worldWidth plateId = if length xRanges == 2 then fst $ xRanges !! 1 else fst $ xRanges !! 0
                                        where xs = L.map pointX $ getPoints ownerMap plateId
                                              --wrapping = xs `elem` 0 && xs `elem` (width-1)
                                              xRanges = ranges xs


--plateRight :: OwnerMap -> PlateId -> Int
--plateRight ownerMap plateId = maximum . map pointX $ getPoints ownerMap plateId


getPoints :: OwnerMap -> PlateId -> [Point]
getPoints ownerMap plateId = L.map fst . filter (\(point,id) -> id==plateId) $ M.assocs ownerMap
