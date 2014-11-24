{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE ScopedTypeVariables #-}

module PlatesGenerationTest where

import Test.Framework
import PlatesGeneration
import qualified Data.Array.Repa as R
import qualified HeightMap.Base as HB
import qualified Data.Map.Strict as M
import qualified Data.List as L
import Geometry
import Control.Monad

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

test_toElevationMap = do
    heightMap <- generateInitialHeightMap 1 100 200
    let elevationMap = toElevationMap heightMap
    let keys = L.sort $ M.keys elevationMap
    let expectedKeys = L.sort [Point x y | x <- [0..99], y <- [0..199] ]
    assertEqual (100*200) (length keys)
    forM expectedKeys (\ek ->
        assertEqualVerbose ("Missing "++ show ek) True (ek `elem` keys))

test_hbMapWidth = do
    heightMap <- generateInitialHeightMap 1 123 456
    assertEqual 123 (hbMapWidth heightMap)

test_hbMapHeight = do
    heightMap <- generateInitialHeightMap 1 123 456
    assertEqual 456 (hbMapHeight heightMap)