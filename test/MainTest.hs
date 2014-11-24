{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Main where

import Test.Framework
import Test.Framework.BlackBoxTest
import {-@ HTF_TESTS @-} PlateTest
import {-@ HTF_TESTS @-} PlatesGenerationTest

main = htfMain htf_importedTests
