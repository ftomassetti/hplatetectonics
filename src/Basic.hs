module Basic where

import System.Random
import qualified Data.List as L

type PlateId = Int

-- Generate a random angle
randAngle :: IO Float
randAngle = do
    r <- (randomRIO (0.0, 1.0) :: IO Float)
    return $ r*2*pi

-- Generate multiple random angles
randAngles :: Int -> IO [Float]
randAngles n = sequence $ replicate n randAngle


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
