module Geometry where

import System.Random
import qualified HeightMap.Base as HB
import qualified Data.Map.Strict as M
import Basic
import Data.Maybe

data Point = Point { pointX :: Int, pointY :: Int }
             deriving (Eq, Ord)

type Angle = Float

instance Show Point where
  show point = "(" ++ (show $ pointX point) ++ "," ++ (show $ pointY point) ++ ")"


randomPoint width height = do x <- randomRIO (0, (width-1))  :: IO Int
                              y <- randomRIO (0, (height-1)) :: IO Int
                              return $ Point x y

randomDinstinctPoints :: Int -> Int -> Int -> IO [Point]
randomDinstinctPoints width height nPoints
    | (width*height) < nPoints = error "Cannot extract so many distinct points from such a map"
    | otherwise = randomDistinctPoints' width height nPoints []
randomDistinctPoints' width height 0       points = return points
randomDistinctPoints' width height nPoints points = do point  <- randomPoint width height
                                                       if point `elem` points
                                                       then randomDistinctPoints' width height nPoints points
                                                       else randomDistinctPoints' width height (nPoints-1) (point:points)

toroidalNorth width height point = let y = (pointY point) - 1
                                   in if  y < 0 then point { pointY = y + height } else point { pointY = y }

toroidalSouth width height point = let y = (pointY point) + 1
                                   in if  y >= height then point { pointY = y - height } else point { pointY = y }

toroidalEast width height point = let x = (pointX point) + 1
                                   in if  x >= width then point { pointY = x - width } else point { pointX = x }

toroidalWest width height point = let x = (pointX point) - 1
                                   in if  x < 0 then point { pointX = x + width } else point { pointX = x }


data WorldDimension = WorldDimension { worldWidth :: Int, worldHeight :: Int }

type ElevationMap = M.Map Point Float

getElevation :: ElevationMap -> Point -> Float
getElevation elevMap point = fromJust $ M.lookup point elevMap