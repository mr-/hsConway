{-# LANGUAGE NoMonomorphismRestriction, FlexibleInstances, TypeSynonymInstances#-}

module Conway where
import Data.Array
import Data.List (unlines, lines, intersperse)




data Cell = Dead | Alive deriving (Eq, Show)
type Grid = Array (Int,Int) Cell

data Delta = Kill | Spawn | KeepDead | KeepAlive deriving (Eq, Show)
type GridDelta = Array (Int,Int) Delta



deltaGridList grid = zipWith deltaGrid l1 l
  where l = gridList grid
        l1 = (emptyGrid grid):l

--deltaGridList grid = (deltaGrid (emptyGrid grid) ((gridList grid) !! 0)) : 
--  [(deltaGrid ((gridList grid) !! (n-1)) ((gridList grid) !! n)) | n <- [1..] ]

--deltaGridList grid = deltaGridList' (gridList grid)
--where deltaGridList' []        = []
--      deltaGridList' (x:[])    = [deltaGrid (emptyGrid x) x]
--      deltaGridList' (x:y:xs)  = (deltaGrid x y) : (deltaGridList' xs)

emptyGrid grid = listArray c (map (\x -> Dead) (range c))
      where  c = bounds grid

deltaGrid :: Grid -> Grid -> GridDelta
deltaGrid oldGrid newGrid = listArray c (map f (range c))
            where c = bounds newGrid
                  f (a,b) = same (oldGrid ! (a,b)) (newGrid ! (a,b)) 
                  same Alive Alive = KeepAlive
                  same Dead Dead   = KeepDead
                  same Dead Alive  = Spawn
                  same Alive Dead  = Kill

gridList grid = iterate nextGen grid


neighbourCount grid x y = length $ filter (==Alive) $ neighbours grid x y 


neighbours :: Grid -> Int -> Int -> [Cell]
neighbours grid x y = arrayElements grid ns
    where ns = [ (wrap (x+x') x2, wrap (y+y') y2) | x' <- [-1, 0, 1], 
                                                    y' <- [-1, 0, 1], 
                                                    not(x'==0 && y' == 0)]
          ((x1,y1),(x2,y2)) = bounds grid
          wrap a bou | (a < 1)   = bou
                     | (a > bou) = 1
                     | otherwise = a
          arrayElements grid indices = map (grid !) indices 


traverseFilter g tr kd = map tr $ filter kd (assocs g)

dim :: Grid -> (Int, Int)
dim = snd.bounds

action :: Cell -> Int -> Cell
action Alive nghc 
    | nghc <= 1 = Dead
    | nghc == 2 = Alive
    | nghc == 3 = Alive
    | nghc > 3  = Dead
action Dead nghc
    | nghc == 3 = Alive 
    | otherwise = Dead

nextGen :: Grid -> Grid
nextGen gr = listArray b $ map f $ assocs gr
  where b = bounds gr
        f ((x,y), v) = action v (neighbourCount gr x y )

swapGrid :: Int -> Int -> Grid -> Grid
swapGrid x y gr = gr // [((x, y), newCell)]
            where newCell = invert ( gr ! (x,y) )
                  invert Alive = Dead
                  invert Dead  = Alive 


stringToGrid :: [String] -> Grid
stringToGrid s = listArray ((1,1), (h, w)) $ linearize s -- (1,1) (1,2) (1,3) .. (2,1) ..
        where w = length (head s)
              h = length s
              linearize s = concat $ map (map trans) s
              trans '.' = Dead
              trans _   = Alive

instance Show Grid where
  show = unlines.gridToString

gridToString :: Grid -> [String]
gridToString gr = map l [1..height]
        where ((a,b), (height, width)) = bounds gr
              l i = gridLineString gr i
              gridLineString gr i = concatMap trans [ gr ! (i, x) | x <- [1..width]]
              trans Dead = "."
              trans Alive  = "#"

instance Show GridDelta where
  show = unlines.deltaToString

deltaToString :: GridDelta -> [String]
deltaToString gr = map l [1..height]
        where ((a,b), (height, width)) = bounds gr
              l i = gridLineString gr i
              gridLineString gr i = concatMap trans [ gr ! (i, x) | x <- [1..width]]
              trans KeepDead = "."
              trans KeepAlive  = "#"
              trans Spawn = "#"
              trans Kill = "."


