import Data.Array.Unboxed
import Data.List
type Grid = UArray (Int,Int) Bool



neighbours :: Grid -> Int -> Int -> [Bool]
neighbours grid x y = map (\x -> grid ! x ) ns
    where ns = [ (x', y') | x' <- [x-1, x, x+1], y' <- [y-1, y, y+1], not(x'==x && y' == y), inbounds (x', y')]
          ((x1,y1),(x2,y2)) = bounds grid
          inbounds (a,b)    = x1 <= a && a <= x2 && y1 <= b && b <= y2


neighbourCount :: Grid -> Int -> Int -> Int
neighbourCount grid x y  = sum (map trans (neighbours grid x y))
                    where trans False = 0
                          trans True  = 1

action :: Bool -> Int -> Bool
action True nghc 
	| nghc <= 1 = False
	| nghc == 2 = True
	| nghc == 3 = True
	| nghc > 3  = False
action False nghc
	| nghc == 3 = True 
	| otherwise = False

nextGen :: Grid -> Grid
nextGen gr = listArray b (map f (range b))
                where b = bounds gr
                      f (x,y) = action (gr ! (x,y)) (neighbourCount gr x y)


stringToGrid :: [String] -> Grid
stringToGrid s = listArray ((1,1), (height, width)) $ linearize s -- (1,1) (1,2) (1,3) .. (2,1) ..
        where width = length (head s)
              height = length s

linearize s = concatMap f s
        where f         = map trans
              trans '.' = False
              trans _   = True

gridToString :: Grid -> [String]
gridToString gr = map l [1..height]
        where ((a,b), (height, width)) = bounds gr
              l i = gridLineString gr i

              gridLineString gr i = concatMap trans [ gr ! (i, x) | x <- [1..width]]
              trans False = "."
              trans True  = "#"

str gr = (intercalate "\n" (gridToString gr))

conc (x:xs) a = r:(conc xs r)
    where r = x a
conc [] a = [] 

doIt n gr = mapM (\x -> putStr $ (str x) ++ "\n\n") $ take n $ conc (repeat nextGen) gr
doIt' n gr = mapM (\x -> putStr $ (str x)++"\n\n") $ take n $ unfoldr (\x -> Just (nextGen x, nextGen x)) gr  

glider = stringToGrid
   ["............",
    "..........#.",
    "..........#.",
    "..........#.",
    "............",
    "............",
    "............",
    "............",
    "............",
    "............",
    "............",
    ".......###..",
    ".......#....",
    "........#...",
    "............"]
blinker = stringToGrid
   [".#.",
    ".#.",
    ".#."]