module Main where

import Data.Maybe
import System.Random
import qualified Data.Map.Strict as M

data Point = Point { pointX :: Int, pointY :: Int }
             deriving (Eq, Ord)

instance Show Point where
  show point = "(" ++ (show $ pointX point) ++ "," ++ (show $ pointY point) ++ ")"

data Plate = Plate { plateExplorableBorders :: [Point] }
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
    let plates = map (\p -> Plate [p]) points
    let plates' = foldl (\m p -> M.insert (M.size m) p m) M.empty plates
    expandPlates width height plates'

main = do let seed   = 1
          setStdGen $ mkStdGen seed
          let width  = 5
          let height = 5
          (owners,plates) <- generatePlates width height 3
          --putStrLn $ "Plates: " ++ (show plates)
          --putStrLn $ "Owners: " ++ (show owners)
          --putStrLn $ "Owners: " ++ (show $ M.size owners)
          return ()


