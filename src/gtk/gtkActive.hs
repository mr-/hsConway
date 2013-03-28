{-# LANGUAGE NoMonomorphismRestriction, FlexibleInstances #-}

--implement movie lazily


import Graphics.Rendering.Cairo

import Graphics.UI.Gtk
import Conway 
import Data.Array
import Data.IORef
import Data.Maybe
import Data.List (unlines, lines, intersperse, foldl')
import Control.Monad (forM_, when, unless, liftM,mplus)
import System.Environment (getArgs)
import System.Mem
import Data.Active
import Data.Semigroup

instance Semigroup (Render ()) where
  (<>) = (>>)

type Universe = [Grid]

cellSize = 10
main :: IO ()
main= do
    initGUI
    window <- windowNew
    table   <- tableNew 20 10 True
    startButton <- buttonNewWithLabel "Start"
    pauseButton <- buttonNewWithLabel "Pause"
    mFilename <- maybeFirstArg
    let defaultfilename = Just "../../universes/line.conway"
    g <- gliderFromFile (fromJust $ mplus mFilename defaultfilename)
    let (width, height) = dim g
        w = fromIntegral $ cellSize*width
        h = fromIntegral $ cellSize*height
    set window [windowTitle := "hsConway",
                windowDefaultWidth := (w+100), windowDefaultHeight := (h ),
                containerBorderWidth := 10 ]

    containerAdd window table

    canvas <- drawingAreaNew

    tableAttachDefaults table startButton 0 1 1 2
    tableAttachDefaults table pauseButton 1 2 1 2


    tableAttachDefaults table canvas 3 10 0 20

    widgetModifyBg canvas StateNormal (Color 0 0 0)

    widgetShowAll window 

    drawin <- widgetGetDrawWindow canvas



    onClicked startButton ( do  aniID <- startAnimation drawin (runActive $ movie (take 50 (animationList g)))
                                onClicked pauseButton ( timeoutRemove aniID)
                                return ()
                          )

    onDestroy window mainQuit
    mainGUI


defaultSpeed = 100
startAnimation drawin animation  = do
        timeRef <- newIORef (0)
        putStrLn "Started Animation"
        timerID <- timeoutAdd( do time <- readIORef timeRef
                                  drawWindowClear drawin
                                  putStrLn $ show time
                                  renderWithDrawable drawin ((animation (toTime time)) >> fill)
                                  modifyIORef timeRef (+0.1)
                                  return True ) ( defaultSpeed )
        return timerID
    



maybeFirstArg = do list <- getArgs 
                   let z = bang list 
                   return z
      where bang  [] = Nothing
            bang  (x:xs) = Just x

gliderFromFile filename = do content <- readFile filename 
                             return $ stringToGrid $ lines content



centerCoords :: Int -> Int -> (Double, Double)
centerCoords a b = ( fromIntegral nx,  fromIntegral ny )
        where nx = cellSize * a - div cellSize 2
              ny = cellSize * b - div cellSize 2


cell rx ry = rectangle (rx - (cellSize/2)) (ry -(cellSize/2))  (cellSize) (cellSize)
redish t = setSourceRGBA t 0 0 0.8
--spawn :: Int -> Int -> Double -> Render ()
spawn x y t = do
    let (rx, ry) = centerCoords x y 
    cell rx ry
    redish $ fromTime t
    fill
--kill :: Int -> Int -> Double -> Render ()
kill x y t = do
    let (rx, ry) = centerCoords x y 
    cell rx ry
    redish (1 - (fromTime t))
    fill
--keepAlive :: Int -> Int -> Double -> Render ()
keepAlive x y t = do
    let (rx, ry) = centerCoords x y 
    cell rx ry
    setSourceRGBA 1 0 0 0.8
    fill
keepDead x y t = return ()




--ft :: Double
ft = 1
z = toTime 0
o = toTime 1
--transf :: GridDelta -> [Animation]
transf g = map tr $ filter kd (assocs g)
  where tr ((x,y), Kill)      = mkActive z o (kill  x y) 
        tr ((x,y), Spawn)     = mkActive z o (spawn x y)
        tr ((x,y), KeepDead)  = mkActive z o (keepDead x y)
        tr ((x,y), KeepAlive) = mkActive z o (keepAlive x y)
        kd ((x,y), KeepDead) = False
        kd _ = True


--animationList ::  [Animation]
animationList gr = 
  map (\g -> foldl (<>) (mkActive 0 0 (\t -> return ())) (transf g) ) (deltaGridList gr)
-- Slower if animationList depends on grid?

{-
data Animation = A (Double, Double -> Picture)
type Picture = Render ()
instance Show Animation where
  show (A (i, f)) = "A " ++ (show i) ++ "f" 

combConst :: [Animation] -> Picture -> [Animation]
combConst l pic = map tr l
  where tr (A (t, f)) = A (t, combine [f, (\t -> pic)])

funcFromAnimation (A (x, f)) = f
timeFromAnimation (A (x, f)) = x

transformAnimation :: (Picture -> Picture) -> Animation -> Animation
transformAnimation tr an = A(x, \t -> tr (f t) )
   where f = funcFromAnimation an 
         x = timeFromAnimation an 

transformAnimations :: (Picture -> Picture) -> [Animation] -> [Animation]
transformAnimations tr an = map (transformAnimation tr) an

anim :: [Animation] -> Double -> Picture
anim l t  = a (t - start)
  where ((start, end), a) = fromDur t l

fromDur :: Double -> [Animation] -> ((Double, Double), Double -> Picture)
fromDur t l = head $ filter (\((s,e), f) -> and [(s <= t),(t <= e)] ) (totalFromDur l)

totalFromDur :: [Animation] -> [((Double, Double), Double -> Picture)]
totalFromDur l = scanl calc ((0,s),abc) (tail l)
    where A (s, abc) = head l
          calc ( (startac, endac), fac) (A (ordur, for)) = ((endac, ordur + endac), for)


combAnim :: [Animation] -> Animation
combAnim l = A (m, combine l')
  where m  = maximum $ map timeFromAnimation l
        l' = map funcFromAnimation l 

combine :: [(Double -> Picture)] -> Double -> Picture
combine l t = foldl' (>>) (return ()) (map (\x -> x t) l)
-}
