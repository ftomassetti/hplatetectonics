{-# LANGUAGE ScopedTypeVariables #-}

module PlatesGeneration where

import System.Random
import qualified Data.Map.Strict as M
import qualified HeightMap.Base as HB
import qualified Data.List as L
import qualified Data.Array.Repa as R
import Data.Word (Word8)
import Basic
import Geometry
import Plate
import Data.Maybe

generateInitialHeightMap :: Int -> Int -> Int -> IO (HB.HeightMap (HB.Point Float))
generateInitialHeightMap seed width height = do
  let rg = mkStdGen seed

  seed1 <- randomRIO (0.0, 1.0) :: IO Float
  seed2 <- randomRIO (0.0, 1.0) :: IO Float
  seed3 <- randomRIO (0.0, 1.0) :: IO Float
  seed4 <- randomRIO (0.0, 1.0) :: IO Float

  let heightMap = HB.unitHeightMap rg (height,width) seed1 seed2 seed3 seed4
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


data Cell = Cell { elevation :: Float, owner :: Maybe PlateId, age :: Int }
type WorldMap = M.Map Point Cell

createCell = Cell 0 Nothing 0
allPoints world height = [Point x y | x <- [0..world], y <- [1..height]]

simulationStep :: Int -> Int -> [Plate] ->[Plate]
simulationStep width height plates =    let totalVelocity       = L.foldr (\p acc -> acc + plateVelocity p) 0 plates
                                            momenta             = L.map plateMomentum plates
                                            systemKineticEnergy = L.foldr (+) 0 momenta
                                            maxKineticEnergy    = maximum momenta
                                            -- TODO restart part
                                            plates'             = L.map resetSegments plates
                                            -- TODO erosion part
                                            plates''            = L.map movePlate plates'
                                            initialWorld        = foldl (\m p -> M.insert p createCell m) M.empty (allPoints width height)
                                            world               = foldl processPlate initialWorld plates'
                                        in plates''
                                        where processPlate world plate = world

-- Visible for testing
hbMapWidth :: HB.HeightMap (HB.Point Float) -> Int
hbMapWidth hbMap = L.length $ L.nub $ L.map HB.getX points
                   where points = R.toList hbMap

-- Visible for testing
hbMapHeight :: HB.HeightMap (HB.Point Float) -> Int
hbMapHeight hbMap = L.length $ L.nub $ L.map HB.getY points
                    where points = R.toList hbMap

toElevationMap :: HB.HeightMap (HB.Point Float) -> ElevationMap
toElevationMap hbMap = L.foldl' addToMap M.empty points
                       where points = R.toList hbMap
                             xs = L.sort $ L.nub $ L.map HB.getX points
                             ys = L.sort $ L.nub $ L.map HB.getY points
                             addToMap :: ElevationMap -> HB.Point Float -> ElevationMap
                             addToMap elevMap (HB.Point (x,y,e)) = M.insert (Point ix iy) e elevMap
                                                                   where ix :: Int = fromJust $ x `L.elemIndex` xs
                                                                         iy :: Int = fromJust $ y `L.elemIndex` ys

