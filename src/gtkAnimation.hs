{-# LANGUAGE NoMonomorphismRestriction #-}

import Graphics.Rendering.Cairo

import Graphics.UI.Gtk
import Conway 
import Data.Array
import Data.IORef
import Data.Maybe
import Data.List (unlines, lines, intersperse)
import Control.Monad (forM_, when, unless, liftM,mplus)
import System.Environment (getArgs)
import System.Mem

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
    let defaultfilename = Just "../universes/line.conway"
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



    onClicked startButton ( do  aniID <- startAnimation drawin (anim   (animationList g))
                                onClicked pauseButton ( timeoutRemove aniID)
                                return ()
                          )

    onDestroy window mainQuit
    mainGUI

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



spawn :: Int -> Int -> Double -> Render ()
spawn x y t = do
    let (rx, ry) = centerCoords x y 
    rectangle (rx - (cellSize/2)) (ry -(cellSize/2))  (cellSize) (cellSize)
    setSourceRGBA t 0 0 0.8
    fill
kill :: Int -> Int -> Double -> Render ()
kill x y t = do
    let (rx, ry) = centerCoords x y 
    rectangle (rx - (cellSize/2)) (ry -(cellSize/2))  (cellSize) (cellSize)
    setSourceRGBA (1-t) 0 0 0.8
    fill
keepAlive :: Int -> Int -> Double -> Render ()
keepAlive x y t = do
    let (rx, ry) = centerCoords x y 
    rectangle (rx - (cellSize/2)) (ry -(cellSize/2))  (cellSize) (cellSize)
    setSourceRGBA 1 0 0 0.8
    fill
keepDead x y t = return ()


defaultSpeed = 50
startAnimation drawin animation  = do
        timeRef <- newIORef (0)
        timerID <- timeoutAdd( do  time <- readIORef timeRef
                                   drawWindowClear drawin
                                   renderWithDrawable drawin (animation time)
                                   modifyIORef timeRef (+0.1)
                                   performGC
                                   return True ) ( defaultSpeed )
        return timerID
    

ft :: Double
ft = 1
transf :: GridDelta -> [Animation]
transf g = map tr $ filter kd (assocs g)
  where tr ((x,y), Kill)      = A (ft, kill  x y) 
        tr ((x,y), Spawn)     = A (ft, spawn x y)
        tr ((x,y), KeepDead)  = A (ft, keepDead x y)
        tr ((x,y), KeepAlive) = A (ft, keepAlive x y)
        kd ((x,y), KeepDead) = False
        kd _ = True


--animationList ::  [Animation]
animationList gr = map (\g -> combAnim (transf g) ) (deltaGridList gr)
-- Slower if animationList depends on grid?


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
combine l t = foldl (>>) (return ()) (map (\x -> x t) l)

 
