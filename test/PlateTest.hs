{-# OPTIONS_GHC -F -pgmF htfpp #-}

module PlateTest where

import Test.Framework
import qualified Basic as B


test_ranges = do
    assertEqual [(0,10)] (B.ranges [0..10])
    assertEqual [(2,4),(7,8),(10,10)] (B.ranges [2,3,4,7,8,10])