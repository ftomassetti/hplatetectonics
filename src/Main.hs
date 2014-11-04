{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Data.Maybe
import System.Random
import qualified Data.Map.Strict as M
import Graphics
import qualified HeightMap.Base as HB
import qualified HeightMap.Mesh as HM
import qualified Data.List as L
import qualified Data.Array.Repa as R
import Data.Array.Repa hiding ((++),map)
import Data.Word (Word8)
import Data.Array.Repa.IO.BMP

instance Show Point where
  show point = "(" ++ (show $ pointX point) ++ "," ++ (show $ pointY point) ++ ")"

data Plate = Plate { plateExplorableBorders :: [Point], plateVelocity :: Int, plateMomentum :: Int }
             deriving (Show)

type PlateId = Int

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

--randomFromList [] =
--randomFromList l  = do let len = length l
--                       index <- randomRIO (0, (len-1))

type OwnerMap = M.Map Point PlateId
type PlatesMap = M.Map PlateId Plate

toroidalNorth width height point = let y = (pointY point) - 1
                                   in if  y < 0 then point { pointY = y + height } else point { pointY = y }

toroidalSouth width height point = let y = (pointY point) + 1
                                   in if  y >= height then point { pointY = y - height } else point { pointY = y }

toroidalEast width height point = let x = (pointX point) + 1
                                   in if  x >= width then point { pointY = x - width } else point { pointX = x }

toroidalWest width height point = let x = (pointX point) - 1
                                   in if  x < 0 then point { pointX = x + width } else point { pointX = x }


-- While at least one plate has a non empty explorableBorders
-- expandPlates
expandPlates :: Int -> Int -> PlatesMap -> IO (OwnerMap, PlatesMap)
expandPlates width height plates = do
    let owners = initialOwners (M.assocs plates) M.empty
    --putStrLn $ "Initial owners: " ++ (show owners)
    helper owners plates
    where initialOwners :: [(PlateId,Plate)] -> OwnerMap -> OwnerMap
          initialOwners [] owners = owners
          initialOwners ((id,plate):plateAssocs) owners = let point = head (plateExplorableBorders plate)
                                                          in initialOwners plateAssocs (M.insert point id owners)
          helper :: OwnerMap -> PlatesMap -> IO (OwnerMap, PlatesMap)
          helper owners plates = let keepGoing = any (\p -> not $ null (plateExplorableBorders p)) (M.elems plates)
                                 in  if keepGoing
                                     then do (owners', plates') <- expandAll owners plates 0
                                             helper owners' plates'
                                     else do --putStrLn $ "End condition : " ++ show plates
                                             return (owners, plates)
          expandAll :: OwnerMap -> PlatesMap -> PlateId -> IO (OwnerMap, PlatesMap)
          expandAll owners plates i =    do --putStrLn $ "Expanding plate " ++ (show i)
                                            (owners',plates') <- expandSingle owners plates i
                                            if i==(M.size plates) -1
                                            then return (owners', plates')
                                            else expandAll owners' plates' (i+1)
          expandSingle :: OwnerMap -> PlatesMap -> PlateId -> IO (OwnerMap, PlatesMap)
          expandSingle owners plates id = do
              let p = fromJust $ M.lookup id plates
              let borders = plateExplorableBorders p
              if null borders
              then return (owners, plates)
              else do borderIndex <- randomRIO (0, length borders - 1)
                      let borderPoint = borders !! borderIndex
                      --putStrLn $ "Expanding single plate " ++ (show id) ++ " index " ++ show borderIndex ++ " point " ++ show borderPoint

                      -- explore in all directions
                      let (owners', plates')       = expandIn owners plates id (toroidalNorth width height borderPoint)
                      let (owners'', plates'')     = expandIn owners' plates' id (toroidalSouth width height borderPoint)
                      let (owners''', plates''')   = expandIn owners'' plates'' id (toroidalEast width height borderPoint)
                      let (owners'''', plates'''') = expandIn owners''' plates''' id (toroidalWest width height borderPoint)

                      --putStrLn $ "  north -> owners = " ++ (show owners') ++ " plates = " ++ (show plates')
                      --putStrLn $ "  south -> owners = " ++ (show owners'') ++ " plates = " ++ (show plates'')
                      --putStrLn $ "  east -> owners = " ++ (show owners''') ++ " plates = " ++ (show plates''')
                      --putStrLn $ "  west-> owners = " ++ (show owners'''') ++ " plates = " ++ (show plates'''')

                      -- remove the point
                      let removedBorders = filter (\p -> p /= borderPoint)  (plateExplorableBorders (fromJust $ M.lookup id plates''''))
                      let p' = p { plateExplorableBorders = removedBorders }
                      let plates''''' = M.insert id p' plates''''

                      --putStrLn $ "  -> owners = " ++ (show owners'''') ++ " plates = " ++ (show plates''''')

                      return (owners'''', plates''''')

          expandIn :: OwnerMap -> PlatesMap -> PlateId -> Point -> (OwnerMap, PlatesMap)
          expandIn owners plates id point =
            if M.member point owners
            then (owners, plates) -- already owned, nothing to do
            else let owners' = M.insert point id owners
                     plate = fromJust $ M.lookup id plates
                     borders' = point:plateExplorableBorders plate
                     plate' = plate { plateExplorableBorders = borders' }
                     plates' = M.insert id plate' plates
                  in (owners',plates')

generatePlates :: Int -> Int -> Int -> IO (OwnerMap, PlatesMap)
generatePlates width height nplates = do
    points <- randomDinstinctPoints width height nplates
    let plates = map (\p -> Plate [p] 0 0) points
    let plates' = foldl (\m p -> M.insert (M.size m) p m) M.empty plates
    expandPlates width height plates'

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

main = do let seed   = 1
          setStdGen $ mkStdGen seed

          let width  = 512
          let height = 512

          heightMap <- generateInitialHeighMap seed width height

          let seaLevel = findQuantile width height heightMap 0.65
          putStrLn $ "Sea level " ++ show seaLevel

          let heightMap' = polarize seaLevel 0.1 1.0 heightMap

          let hm = HB.reify $ R.map (\p -> float2bytes $ HB.getHeight p)
                           $ heightMap'

          writeImageToBMP "polarized" hm

          (owners,plates) <- generatePlates width height 20
          saveMap width height owners "plates.png"
          --putStrLn $ "Plates: " ++ (show plates)
          --putStrLn $ "Owners: " ++ (show owners)
          --putStrLn $ "Owners: " ++ (show $ M.size owners)
          return ()


