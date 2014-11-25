{-# LANGUAGE ScopedTypeVariables #-}

module Lithosphere where

import Basic
import Geometry
import Plate
import qualified Data.Map.Strict as M
import Data.Maybe
import System.Random
import qualified Data.List as L

type OwnerMap = M.Map Point PlateId

getPointsOwned :: PlateId -> OwnerMap -> [Point]
getPointsOwned id = M.keys . M.filterWithKey (\p id' -> id == id')

data PlateTectonicsProcess = PlateTectonicsProcess {
    -- determines the amount of iterations, i.e. the number of
    -- consecutive calls to update method that should pass between sequential runs
    -- of the erosion algorithm. A value of zero would mean that erosion is never
    -- applied to the system
    procErosionPeriod :: Int,

    -- is the amount of crust that is moved from the source plate
    -- to the destination plate at the location where two plates overlap. A value of 0.0
    -- would mean that nothing is transferred, 1.0 causes everything to be transferred
    -- immediately
    procFoldingRatio :: Float,

    -- is an abbreviation of "absolute aggregation ratio". In short
    -- it defines the minimum amount of surface area that one continent on a plate
    -- must overlap with another continent on another plate during that iteration be-
    -- fore the two continents are merged into one continent that resides on either
    -- plate. The smaller continent will always be merged to the larger on
    procAggRatioAbs :: Float,

    -- defines the minimum percentage of surface area of a continent that must overlap
    -- with another continent before they are merged into one
    -- continent. A value of 0.0 would cause two colliding continents to become
    -- merged immediately. A value of 1.0 requires that the smaller plate is complete-
    -- ly beneath the larger plate before they are merged together. This parameter is
    -- in a central role when the rate of continental growth is adjusted
    procAggRatioRel :: Float,

    -- sets the maximum number of times that the lithosphere is divided into plates.
    -- If zero, then new set of plates are generated instead of the old
    -- ones every time the sum of plate movement slows down too much
    procNumCycles :: Int
}

-- The Lithosphere module manages the interactions between single plates
-- and it is able to calculate the complexive heightmap
--
-- The lithosphere tracks collissions and subductions

data Lithosphere = Lithosphere {
    lithoWorldDim    :: WorldDimension,
    lithoPlates      :: PlatesMap,
    lithoCollisions  :: [Collision],
    lithoSubductions :: [Subduction]
}

data Collision = Collision

data Subduction = Subduction

generateLithosphere :: WorldDimension -> ElevationMap -> Int -> IO Lithosphere
generateLithosphere worldDim elevation nPlates = do
  let width = worldWidth worldDim
  let height = worldHeight worldDim
  platesMap <- generatePlates width height elevation nPlates
  return $ Lithosphere worldDim platesMap [] []

---
--- Creating plates
---

-- Structure used just when building the plates
data PlateBuilder = PlateBuilder { plateExplorableBorders :: [Point] }

createPlateBuilder p = PlateBuilder [p]

type PlateBuildersMap = M.Map PlateId PlateBuilder


-- While at least one plate has a non empty explorableBorders
-- expandPlates
expandPlates :: Int -> Int -> ElevationMap -> PlateBuildersMap -> IO PlatesMap
expandPlates width height elevMap plates = do
    let owners :: OwnerMap = initialOwners (M.assocs plates) M.empty
    (owners',plates') <- helper owners plates
    angles <- randAngles (M.size plates)
    let platesRes = M.fromList $ map (\((id,_),angle) -> (id,createPlate' id angle owners' elevMap)) (zip (M.toList plates') angles)
    return platesRes
    where createPlate' :: PlateId -> Angle -> OwnerMap -> ElevationMap -> Plate
          createPlate' id angle owners elevMap  = createPlate id angle cells
                                                  where points :: [Point] = getPointsOwned id owners
                                                        createCell :: Point -> ElevationMap -> PlateCell
                                                        createCell point elevMap = PlateCell el 0
                                                                                   where el :: Float = getElevation elevMap point
                                                        cells = L.foldr (\p m -> M.insert p (createCell p elevMap) m) M.empty points

          initialOwners :: [(PlateId,PlateBuilder)] -> OwnerMap -> OwnerMap
          initialOwners [] owners = owners
          initialOwners ((id,plate):plateAssocs) owners = let point = head (plateExplorableBorders plate)
                                                          in initialOwners plateAssocs (M.insert point id owners)
          helper :: OwnerMap -> PlateBuildersMap -> IO (OwnerMap, PlateBuildersMap)
          helper owners plates = let keepGoing = any (\p -> not $ null (plateExplorableBorders p)) (M.elems plates)
                                 in  if keepGoing
                                     then do (owners', plates') <- expandAll owners plates 0
                                             helper owners' plates'
                                     else do return (owners, plates)
          expandAll :: OwnerMap -> PlateBuildersMap -> PlateId -> IO (OwnerMap, PlateBuildersMap)
          expandAll owners plates i =    do (owners',plates') <- expandSingle owners plates i
                                            if i==(M.size plates) -1
                                            then return (owners', plates')
                                            else expandAll owners' plates' (i+1)
          expandSingle :: OwnerMap -> PlateBuildersMap -> PlateId -> IO (OwnerMap, PlateBuildersMap)
          expandSingle owners plates id = do
              let p = fromJust $ M.lookup id plates
              let borders = plateExplorableBorders p
              if null borders
              then return (owners, plates)
              else do borderIndex <- randomRIO (0, length borders - 1)
                      let borderPoint = borders !! borderIndex

                      -- explore in all directions
                      let (owners', plates')       = expandIn owners plates id (toroidalNorth width height borderPoint)
                      let (owners'', plates'')     = expandIn owners' plates' id (toroidalSouth width height borderPoint)
                      let (owners''', plates''')   = expandIn owners'' plates'' id (toroidalEast width height borderPoint)
                      let (owners'''', plates'''') = expandIn owners''' plates''' id (toroidalWest width height borderPoint)

                      -- remove the point
                      let removedBorders = filter (\p -> p /= borderPoint)  (plateExplorableBorders (fromJust $ M.lookup id plates''''))
                      let p' = p { plateExplorableBorders = removedBorders }
                      let plates''''' = M.insert id p' plates''''

                      return (owners'''', plates''''')

          expandIn :: OwnerMap -> PlateBuildersMap -> PlateId -> Point -> (OwnerMap, PlateBuildersMap)
          expandIn owners plates id point =
            if M.member point owners
            then (owners, plates) -- already owned, nothing to do
            else let owners' = M.insert point id owners
                     plate = fromJust $ M.lookup id plates
                     borders' = point:plateExplorableBorders plate
                     plate' = plate { plateExplorableBorders = borders' }
                     plates' = M.insert id plate' plates
                  in (owners',plates')

generatePlates :: Int -> Int -> ElevationMap -> Int -> IO PlatesMap
generatePlates width height elevMap nplates = do
    points <- randomDinstinctPoints width height nplates
    let plates = map (\p -> createPlateBuilder p) points
    let plates' = foldl (\m p -> M.insert (M.size m) p m) M.empty plates
    expandPlates width height elevMap plates'

---
--- Exporting
---

lithoPlatesMapToElevationMap :: Int -> Int ->  PlatesMap -> ElevationMap
lithoPlatesMapToElevationMap w h platesMap = L.foldr addPlate initialMap (M.elems platesMap)
                                             where allPoints :: [Point]
                                                   allPoints = [ Point x y | x <- [0..(w-1)], y <- [0..(h-1)]]
                                                   initPoint :: Point -> ElevationMap -> ElevationMap
                                                   initPoint p m = M.insert p 0.0 m
                                                   initialMap :: ElevationMap
                                                   initialMap = L.foldr initPoint M.empty allPoints
                                                   addPlate :: Plate -> ElevationMap -> ElevationMap
                                                   addPlate plate elevMap = L.foldr addPoint elevMap (platePoints plate)
                                                                            where addPoint :: Point -> ElevationMap -> ElevationMap
                                                                                  addPoint p m = let base :: Float = case M.lookup p m of
                                                                                                                 Nothing -> 0.0
                                                                                                                 Just value -> value
                                                                                                     plateElev :: Float = platePointElevation p plate
                                                                                                     newValue :: Float = base + plateElev
                                                                                                 in M.insert p newValue m


