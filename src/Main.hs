{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Data.Maybe
import System.Random
import qualified Data.Map.Strict as M
import Graphics
import qualified HeightMap.Base as HB
import qualified Data.List as L
import qualified Data.Array.Repa as R
import Data.Word (Word8)
import Data.Array.Repa.IO.BMP
import Basic
import Plate

-- While at least one plate has a non empty explorableBorders
-- expandPlates
expandPlates :: Int -> Int -> PlateBuildersMap -> IO (OwnerMap, PlatesMap)
expandPlates width height plates = do
    let owners :: OwnerMap = initialOwners (M.assocs plates) M.empty
    (owners',plates') <- helper owners plates
    let platesRes = M.map (\_ -> createPlate) plates'
    return (owners', platesRes)
    where initialOwners :: [(PlateId,PlateBuilder)] -> OwnerMap -> OwnerMap
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

generatePlates :: Int -> Int -> Int -> IO (OwnerMap, PlatesMap)
generatePlates width height nplates = do
    points <- randomDinstinctPoints width height nplates
    let plates = map (\p -> createPlateBuilder p) points
    let plates' = foldl (\m p -> M.insert (M.size m) p m) M.empty plates
    expandPlates width height plates'

generateInitialHeighMap :: Int -> Int -> Int -> IO (HB.HeightMap (HB.Point Float))
generateInitialHeighMap seed width height = do
  let rg = mkStdGen seed

  seed1 <- randomRIO (0.0, 1.0) :: IO Float
  seed2 <- randomRIO (0.0, 1.0) :: IO Float
  seed3 <- randomRIO (0.0, 1.0) :: IO Float
  seed4 <- randomRIO (0.0, 1.0) :: IO Float

  let heightMap = HB.unitHeightMap rg (width,height) seed1 seed2 seed3 seed4
  return heightMap

-- Find the value v such as (quantile*100)% are below that value
findQuantile :: Int -> Int -> HB.HeightMap (HB.Point Float) -> Float -> Float
findQuantile width height heightmap quantile =
    -- First we count how many cells stays in each bucket
    -- where each bucket represent an interval of 0.01
    let points :: [HB.Point Float] = R.toList heightmap
        values = L.sort $ L.map HB.getHeight points
    in values !! target
    where target = round $ fromIntegral (width * height) * quantile

-- take a map and set all the values below the threshold to the low value
-- and all the values above to the high values
polarize :: Float -> Float -> Float -> HB.HeightMap (HB.Point Float) -> HB.HeightMap (HB.Point Float)
polarize th lowValue highValue heightmap = R.computeS $ R.map f heightmap
                                           where f (HB.Point (x,y,value)) = let value' = if value < th then lowValue else highValue
                                                                            in HB.Point (x, y, value')
-- TODO avoid duplicating it
float2bytes :: Float -> (Word8,Word8,Word8)
float2bytes v = (b,b,b) where b = (floor $ 255.0 * v) :: Word8


simulationStep :: [Plate] -> [Plate]
simulationStep plates = let totalVelocity       = L.foldr (\p acc -> acc + plateVelocity p) 0 plates
                            momenta             = L.map plateMomentum plates
                            systemKineticEnergy = L.foldr (+) 0 momenta
                            maxKineticEnergy    = maximum momenta
                            -- TODO restart part
                            plates'             = L.map resetSegments plates
                            -- TODO erosion part
                            plates''            = L.map movePlate plates'
                        in plates''

main = do let seed   = 1
          setStdGen $ mkStdGen seed

          let width  = 512
          let height = 512

          heightMap <- generateInitialHeighMap seed width height

          let seaLevel = findQuantile width height heightMap 0.65
          putStrLn $ "Sea level " ++ show seaLevel

          let heightMap' = polarize seaLevel 0.1 1.0 heightMap

          let hm = HB.reify $ R.map (\p -> float2bytes $ HB.getHeight p)
                           $ heightMap'

          writeImageToBMP "polarized" hm

          (owners,plates) <- generatePlates width height 20
          saveMap width height owners "plates.png"
          --putStrLn $ "Plates: " ++ (show plates)
          --putStrLn $ "Owners: " ++ (show owners)
          --putStrLn $ "Owners: " ++ (show $ M.size owners)
          return ()


