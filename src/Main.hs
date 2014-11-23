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
import Geometry
import Plate
import Lithosphere

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

toElevationMap :: HB.HeightMap (HB.Point Float) -> ElevationMap
toElevationMap hbMap = L.foldr addToMap M.empty points
                       where points = R.toList hbMap
                             width  = L.length $ L.nub $ L.map HB.getX points
                             height = L.length $ L.nub $ L.map HB.getY points
                             fwidth  :: Float = fromIntegral width
                             fheight :: Float = fromIntegral height
                             addToMap :: HB.Point Float -> ElevationMap -> ElevationMap
                             addToMap (HB.Point (x,y,e)) elevMap = M.insert (Point ix iy) e elevMap
                                                                   where ix :: Int = round $ x * fwidth
                                                                         iy :: Int = round $ y * fheight

main = do let seed   = 1
          setStdGen $ mkStdGen seed

          let width  = 512
          let height = 512

          heightMap <- generateInitialHeighMap seed width height

          let seaLevel = findQuantile width height heightMap 0.65
          putStrLn $ "Sea level " ++ show seaLevel

          -- print a map that show the sea level
          let heightMap' = polarize seaLevel 0.1 1.0 heightMap
          let hm = HB.reify $ R.map (\p -> float2bytes $ HB.getHeight p)
                           $ heightMap'
          writeImageToBMP "polarized" hm

          plates :: PlatesMap <- generatePlates width height (toElevationMap heightMap) 15
          --saveMap width height plates "plates.png"

          let elevMap = lithoPlatesMapToElevationMap width height plates

          return ()


