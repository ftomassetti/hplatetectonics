{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import System.Random
import Graphics
import qualified HeightMap.Base as HB
import qualified Data.Array.Repa as R
import Data.Array.Repa.IO.BMP
import Plate
import Lithosphere
import PlatesGeneration

main = do let seed   = 1
          setStdGen $ mkStdGen seed

          let width  = 512
          let height = 512

          heightMap <- generateInitialHeightMap seed width height

          let seaLevel = findQuantile width height heightMap 0.65
          putStrLn $ "Sea level " ++ show seaLevel

          -- print a map that show the sea level
          let heightMap' = polarize seaLevel 0.1 1.0 heightMap
          let hm = HB.reify $ R.map (\p -> float2bytes $ HB.getHeight p)
                           $ heightMap'
          writeImageToBMP "polarized" hm

          plates :: PlatesMap <- generatePlates width height (toElevationMap heightMap) 15

          let elevMap = lithoPlatesMapToElevationMap width height plates
          saveElevMap width height elevMap "elev.png"

          return ()


