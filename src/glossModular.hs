{-# LANGUAGE NoMonomorphismRestriction, FlexibleInstances, TypeSynonymInstances#-}
import Graphics.Gloss.Interface.Pure.Animate
import Data.Array
import Data.List (unlines, lines, intersperse)
import Control.Monad (forM_, when, unless, liftM)
import Conway
import Animation





alist = [A (ft, spawn 10 10),  A (ft, kill 10 10), A (ft, spawn 10 10), A (ft, kill 10 10),  A (ft, spawn 10 10), A (ft, kill 10 10), A (ft, spawn 10 10), A (ft, kill 10 10), A (ft, spawn 10 10), A (ft, kill 10 10), A (ft, spawn 10 10), A (ft, kill 10 10), A (ft, spawn 10 10), A (ft, kill 10 10), A (100000, \t -> blank)]
main = do
  animate ( InWindow "foo" (220,220) (50, 50) ) black 
      (\x -> anim (combConst (animationList ) border) x)


cell x y = color red $ translate ( x) ( y) (circle 4)
foo grid  = translate (-100) (-100) $ pictures $  
   (map (\((x,y),foo) -> cell (fst $ centerCoords x y) (snd $ centerCoords x y) ) $ 
    filter (\((x,y), foo) -> foo == Alive) (assocs grid)) ++ [border]

border = color white $ lineLoop $ rectanglePath  210 210
spawn x y time = color (redish (2*time)) $ translate (-105) (-105) $
      translate (centerX x y) (centerY x y) (rectangleSolid rs rs) 
kill x y  time = color (redish (1-2*time)) $ translate (-105) (-105) $
      translate (centerX x y) (centerY x y) (rectangleSolid rs rs)
keepAlive x y  time = color red $ translate (-105) (-105) $
      translate (centerX x y) (centerY x y) (rectangleSolid rs rs)

spawn' x y time = color (redish (2*time)) $ translate (centerX x y) (centerY x y) (thickCircle 2 4 ) 
kill' x y  time = color (redish (1-2*time)) $ translate (centerX x y) (centerY x y) (thickCircle 2 4 )
keepAlive' x y  time = color red $ translate (centerX x y) (centerY x y) (thickCircle 2 4)
keepDead x y time = blank

redish t = makeColor (t) 0 0 1 


cellSize = 10
rs = cellSize

centerCoords :: Int -> Int -> (Float,Float)
centerCoords a b = ( fromIntegral nx,  fromIntegral ny )
        where nx = cellSize * a - div cellSize 2
              ny = cellSize * b - div cellSize 2

centerX a b = fst $ centerCoords a b 
centerY a b = snd $ centerCoords a b 

ft :: Float
ft = 1
transf :: GridDelta -> [Animation]
transf g = map tr (assocs g)
  where tr ((x,y), Kill)      = A (ft, kill  x y) 
        tr ((x,y), Spawn)     = A (ft, spawn x y)
        tr ((x,y), KeepDead)  = A (ft, keepDead x y)
        tr ((x,y), KeepAlive) = A (ft, keepAlive x y)


animationList ::  [Animation]
animationList  = map (\g -> combAnim (transf g) ) (deltaGridList grid)
-- Slower if animationList depends on grid?


grid3 = stringToGrid ["..........#..........", 
                     "..........#..........", 
                     "..........#..........", 
                     "..........#..........",
                     "..........#..........",
                     "..........#..........",
                     "..........#..........",
                     "..........#..........",
                     "..........#..........",
                     "..........#..........",
                     "..........#..........",
                     "..........#..........",
                     "..........#..........",
                     "..........#..........",
                     "..........#..........",
                     "..........#..........",
                     "..........#..........",
                     "..........#..........",
                     "..........#..........",
                     "..........#..........",
                     "..........#.........."]
grid = stringToGrid ["..........#..........", 
                     "..........#..........", 
                     "..........#..........", 
                     "..........#..........",
                     "..........#..........",
                     "..........#..........",
                     "..........#..........",
                     "..........#..........",
                     "..........#..........",
                     "..........#..........",
                     "#####################",
                     "..........#..........",
                     "..........#..........",
                     "..........#..........",
                     "..........#..........",
                     "..........#..........",
                     "..........#..........",
                     "..........#..........",
                     "..........#..........",
                     "..........#..........",
                     "..........#.........."]

grid2 = stringToGrid ["....................", 
                     ".....#..............", 
                     "....#...............", 
                     "....###.............",
                     "....................",
                     ".........#..........",
                     "........#...........",
                     "........###.........",
                     "....................",
                     "....................",
                     "...............#....",
                     "..............#.....",
                     "..............###...",
                     "....................",
                     "....................",
                     "....................",
                     "....................",
                     "....................",
                     "....................",
                     "...................."]

