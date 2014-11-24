{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE ScopedTypeVariables #-}

module PlatesGenerationTest where

import Test.Framework
import PlatesGeneration
import qualified Data.Array.Repa as R
import qualified HeightMap.Base as HB

test_generateInitialHeightMap = do
    heightMap :: HB.HeightMap (HB.Point Float) <- generateInitialHeightMap 1 512 512
    -- I would expect to hae 512x512 points all beteen 0.0 and 1.0
    let points :: [HB.Point Float] = R.toList heightMap
    let values :: [Float]          = map HB.getHeight points
    assertEqual (512 * 512) (length points)
    let ma :: Float = maximum values
    let mi :: Float = minimum values
    assertEqual True (ma <= 1.0)
    assertEqual True (mi >= 0.0)
    return ()

test_toElevationMap = do
    heightMap <- generateInitialHeightMap 1 512 512
    let elevationMap = toElevationMap heightMap
    return ()

test_hbMapWidth = do
    heightMap <- generateInitialHeightMap 1 123 456
    assertEqual 123 (hbMapWidth heightMap)

test_hbMapHeight = do
    heightMap <- generateInitialHeightMap 1 123 456
    assertEqual 456 (hbMapHeight heightMap)