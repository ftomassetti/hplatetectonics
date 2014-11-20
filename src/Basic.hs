module Basic where

import System.Random
import qualified Data.List as L
import qualified HeightMap.Base as HB


-- given a list it find continguous ranges
ranges :: (Ord a, Enum a) => [a] -> [(a,a)]
ranges [] = []
ranges [x] = [(x,x)]
ranges (x:xs) = buildRanges x x xs
                where sortedXs = L.sort xs
                      buildRanges :: (Ord a, Enum a) => a -> a -> [a] -> [(a,a)]
                      buildRanges start current [] = [(start,current)]
                      buildRanges start current (x:xs) = if x == succ current
                                                         then buildRanges start x xs
                                                         else (start,current):(buildRanges x x xs)

data Point = Point { pointX :: Int, pointY :: Int }
             deriving (Eq, Ord)

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


type ElevationMap = HB.HeightMap (HB.Point Float)

data WorldDimension = WorldDimension { worldWidth :: Int, worldHeight :: Int }