{-# LANGUAGE NoMonomorphismRestriction, FlexibleInstances, TypeSynonymInstances#-}
import Graphics.Gloss.Interface.Pure.Animate
import Data.Array
import Data.List (unlines, lines, intersperse)
import Control.Monad (forM_, when, unless, liftM)
data Cell = Dead | Alive deriving (Eq, Show)

type Grid = Array (Int,Int) Cell

data Animation = A (Float, Float -> Picture)

instance Show Animation where
  show (A (i, f)) = "A " ++ (show i) ++ "f" 

ft :: Float
ft = 1
alist = [A (ft, spawn 10 10),  A (ft, kill 10 10), A (ft, spawn 10 10), A (ft, kill 10 10),  A (ft, spawn 10 10), A (ft, kill 10 10), A (ft, spawn 10 10), A (ft, kill 10 10), A (ft, spawn 10 10), A (ft, kill 10 10), A (ft, spawn 10 10), A (ft, kill 10 10), A (ft, spawn 10 10), A (ft, kill 10 10), A (100000, \t -> blank)]
main = do
  --animate ( InWindow "foo" (220,220) (50, 50) ) black (\x -> foo $ (gridlist $ floor (2*x) )) 
  --animate ( InWindow "foo" (220,220) (50, 50) ) black (\x -> anim alist x)
  animate ( InWindow "foo" (220,220) (50, 50) ) black (\x -> anim (combConst animationList boarder) x)
  --animate ( InWindow "foo" (220,220) (50, 50) ) black (\x -> combine [ spawn 10 10, spawn 5 10 ] x)

--       return ()
cell x y = color red $ translate ( x) ( y) (circle 4)
boarder = translate 100 100 $ color white $ lineLoop $ rectanglePath  200 200

combConst :: [Animation] -> Picture -> [Animation]
combConst l pic = map tr l
  where tr (A (t, f)) = A (t, combine [f, (\t -> pic)])

foo grid  = translate (-100) (-100) $ pictures $  
   (map (\((x,y),foo) -> cell (fst $ centerCoords x y) (snd $ centerCoords x y) ) $ 
    filter (\((x,y), foo) -> foo == Alive) (assocs grid)) ++ [boarder]


spawn x y time = color (redish (1.5*time)) $ translate (centerX x y) (centerY x y) (thickCircle 2 4 ) 
kill x y  time = color (redish (1-1.5*time)) $ translate (centerX x y) (centerY x y) (thickCircle 2 4 )
keepAlive x y  time = color red $ translate (centerX x y) (centerY x y) (thickCircle 2 4)
keepDead x y time = blank

redish t = makeColor (t) 0 0 1 


anim l t  = a (t - start)
  where ((start, end), a) = fromDur t l

fromDur t l = head $ filter (\((s,e), f) -> and [(s <= t),(t <= e)] ) (totalFromDur l)

totalFromDur l = scanl calc ((0,s),abc) (tail l)
    where A (s, abc) = head l
          calc ( (startac, endac), fac) (A (ordur, for)) = ((endac, ordur + endac), for)


stringToGrid :: [String] -> Grid
stringToGrid s = listArray ((1,1), (h, w)) $ linearize s -- (1,1) (1,2) (1,3) .. (2,1) ..
        where w = length (head s)
              h = length s
linearize s = concatMap f s
        where f         = map trans
              trans '.' = Dead
              trans _   = Alive
cellSize = 10
centerX a b = fst $ centerCoords a b 
centerY a b = snd $ centerCoords a b 
centerCoords :: Int -> Int -> (Float,Float)
centerCoords a b = ( fromIntegral nx,  fromIntegral ny )
        where nx = cellSize * a - div cellSize 2
              ny = cellSize * b - div cellSize 2

grid = stringToGrid ["....................", 
                     ".....#..............", 
                     "....#...............", 
                     "....###.............",
                     "....................",
                     "....................",
                     "....................",
                     "....................",
                     "....................",
                     "....................",
                     "....................",
                     "....................",
                     "....................",
                     "....................",
                     "....................",
                     "....................",
                     "....................",
                     "....................",
                     "....................",
                     "...................."]


transf :: GridDelta -> [(Float -> Picture)]
transf g = map tr (assocs g)
  where tr ((x,y), Kill)      = kill  x y 
        tr ((x,y), Spawn)     = spawn x y
        tr ((x,y), KeepDead)  = keepDead x y
        tr ((x,y), KeepAlive) = keepAlive x y


combine :: [(Float -> Picture)] -> Float -> Picture
combine l t = pictures $ map (\a -> a t) l 

animationList :: [Animation]
animationList = map (\g -> A (ft, combine (transf g) ) ) deltaGridList


data Delta = Kill | Spawn | KeepDead | KeepAlive deriving (Eq, Show)
type GridDelta = Array (Int,Int) Delta

deltaGridList = (deltaGrid emptyGrid (gridList !! 0)) : 
  [(deltaGrid (gridList !! (n-1)) (gridList !! n)) | n <- [1..] ]

emptyGrid = listArray c (map (\x -> Dead) (range c))
      where  c = bounds grid

--deltaGridList 0 = deltaGrid emptyGrid grid
--  where emptyGrid = listArray c (map (\x -> Dead) (range c))
--        c = bounds grid
--deltaGridList n = deltaGrid (gridlist (n-1)) (gridlist n)

deltaGrid :: Grid -> Grid -> GridDelta
deltaGrid oldGrid newGrid = listArray c (map f (range c))
            where c = bounds newGrid
                  f (a,b) = same (oldGrid ! (a,b)) (newGrid ! (a,b)) 
                  same Alive Alive = KeepAlive
                  same Dead Dead   = KeepDead
                  same Dead Alive  = Spawn
                  same Alive Dead  = Kill

gridList = iterate nextGen grid


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


{-instance Show GridDelta where
  show foo = unlines (deltaToString foo)
instance Show Grid where
  show foo = unlines (gridToString foo)-}