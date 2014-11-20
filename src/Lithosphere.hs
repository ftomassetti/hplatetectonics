module Lithosphere where

import Basic
import Plate

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
    lithoPlates      :: [Plate],
    lithoCollisions  :: [Collision],
    lithoSubductions :: [Subduction]
}

data Collision = Collision

data Subduction = Subduction

-- complexiveHeightmap: built by summing the contributions of the plates