-- This code is a tentative to implement in Haskell
-- the algorithm brilliantly proposed in a maskter thesis from
-- Viitanen Lauri
-- http://www.theseus.fi/bitstream/handle/10024/40422/Viitanen_Lauri_2012_03_30.pdf

module Plate where

-- import Graphics hiding (Point)
import Geometry
import qualified Data.Map.Strict as M
import qualified HeightMap.Base as HB
import Basic
import Data.Array.Repa
import qualified Data.List as L
--import qualified Data.HashMap as HM
import Data.Maybe

-- A plate is rectangle grid, moving from some location at some speed
-- The position is the top left corner of the plate in the lithosphere
--
-- For each cell of the plate we know both the amount of crust and the age
--
-- We need also to know the total mass and the center of mass of a plate
--
-- Each plate contains multiple Segments.
data Plate = Plate {
                plateId :: PlateId,
                plateAge :: Int,
                plateVelocity  :: Int,
                plateAngle     :: Float,
                plateElevation :: M.Map Point PlateCell,
                plateMomentum  :: Int,
                plateSegments  :: [Segment] }
             deriving (Show)

data PlateCell = PlateCell { cellCrust :: Float, cellAge :: Int } deriving (Show)

-- A segment contains the dimensions and surface area of a single continent
-- within a plate. A continent is defined as a group of adjacent points that are
-- all above the sea level. Each continent has an ID inside the plate
data Segment = Segment
               deriving Show



-----------------------------------------------------------
-- Plate
-----------------------------------------------------------

createPlate :: PlateId -> Angle -> M.Map Point PlateCell -> Plate
createPlate id angle points = Plate id 0 1 angle points 0 []

resetSegments plate = plate

movePlate plate = plate

platePoints plate = M.keys $ plateElevation plate

platePointElevation :: Point -> Plate -> Float
platePointElevation point plate = let cell = fromJust $ M.lookup point (plateElevation plate)
                                  in  cellCrust cell

type PlatesMap = M.Map PlateId Plate

--plateMass :: OwnerMap -> ElevationMap -> PlateId -> Float
--plateMass ownerMap elevMap plateId = foldr (+) 0 elevs
--                                     where points = getPoints ownerMap plateId
--                                           elevs  = L.map (\(Point x y) -> HB.getHeight $ elevMap ! (Z:.x:.y)) points

-- Given it is a toroidal world, the left border is on the point most on the left,
-- unless the plate wraps around the world border
--plateLeft :: OwnerMap -> Int -> PlateId -> Int
--plateLeft ownerMap worldWidth plateId = if length xRanges == 2 then fst $ xRanges !! 1 else fst $ xRanges !! 0
--                                        where xs = L.map pointX $ getPoints ownerMap plateId
                                              --wrapping = xs `elem` 0 && xs `elem` (width-1)
--                                              xRanges = ranges xs


--plateRight :: OwnerMap -> PlateId -> Int
--plateRight ownerMap plateId = maximum . map pointX $ getPoints ownerMap plateId


--getPoints :: OwnerMap -> PlateId -> [Point]
--getPoints ownerMap plateId = L.map fst . filter (\(point,id) -> id==plateId) $ M.assocs ownerMap
