module Conway where
import Graphics.Gloss.Interface.Pure.Animate
import Data.Array
import Data.List (unlines, lines, intersperse)

data Cell = Dead | Alive deriving (Eq, Show)
type Grid = Array (Int,Int) Cell

data Delta = Kill | Spawn | KeepDead | KeepAlive deriving (Eq, Show)
type GridDelta = Array (Int,Int) Delta

deltaGridList grid = (deltaGrid (emptyGrid grid) ((gridList grid) !! 0)) : 
  [(deltaGrid ((gridList grid) !! (n-1)) ((gridList grid) !! n)) | n <- [1..] ]

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

neighbours :: Grid -> Int -> Int -> [Cell]
neighboursB grid x y = map (\x -> grid ! x ) ns
    where ns = [ (x', y') | x' <- [x-1, x, x+1], y' <- [y-1, y, y+1], not(x'==x && y' == y), inbounds (x', y')]
          ((x1,y1),(x2,y2)) = bounds grid
          inbounds (a,b)    = x1 <= a && a <= x2 && y1 <= b && b <= y2

neighbours grid x y = map (\x -> grid ! (f x) ) ns
    where ns = [ (x', y') | x' <- [x-1, x, x+1], y' <- [y-1, y, y+1], not(x'==x && y' == y)]
          ((x1,y1),(x2,y2)) = bounds grid
          f (a, b) = ((c a x2), (c b y2))
          c a bou | (a < 1)   = bou
                  | (a > bou) = 1
                  | otherwise = a

neighbourCount :: Grid -> Int -> Int -> Int
neighbourCount grid x y  = sum (map trans (neighbours grid x y))
                    where trans Dead = 0
                          trans Alive  = 1

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
nextGen gr = listArray b (map f (range b))
                where b = bounds gr
                      f (x,y) = action (gr ! (x,y)) (neighbourCount gr x y)

stringToGrid :: [String] -> Grid
stringToGrid s = listArray ((1,1), (h, w)) $ linearize s -- (1,1) (1,2) (1,3) .. (2,1) ..
        where w = length (head s)
              h = length s
              linearize s = concatMap f s
              f         = map trans
              trans '.' = Dead
              trans _   = Alive

gridToString :: Grid -> [String]
gridToString gr = map l [1..height]
        where ((a,b), (height, width)) = bounds gr
              l i = gridLineString gr i
              gridLineString gr i = concatMap trans [ gr ! (i, x) | x <- [1..width]]
              trans Dead = "."
              trans Alive  = "#"

deltaToString :: GridDelta -> [String]
deltaToString gr = map l [1..height]
        where ((a,b), (height, width)) = bounds gr
              l i = gridLineString gr i
              gridLineString gr i = concatMap trans [ gr ! (i, x) | x <- [1..width]]
              trans KeepDead = "."
              trans KeepAlive  = "#"
              trans Spawn = "#"
              trans Kill = "."