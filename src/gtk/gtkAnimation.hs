{-# LANGUAGE NoMonomorphismRestriction, FlexibleInstances, TypeSynonymInstances#-}

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
import Data.Semigroup
import GeneralAnimation

type Universe = [Grid]

type Animation = Dynamic Double (Render())
instance Semigroup (Render ()) where
    a <> b = a >> b

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



    onClicked startButton ( do  aniID <- startAnimation drawin (anim   (animationList g))
                                onClicked pauseButton ( timeoutRemove aniID)
                                return ()
                          )

    onDestroy window mainQuit
    mainGUI


defaultSpeed = 50
startAnimation drawin animation  = do
        timeRef <- newIORef (0)
        timerID <- timeoutAdd( do time <- readIORef timeRef
                                  drawWindowClear drawin
                                  renderWithDrawable drawin ((animation time) >> fill)
                                  modifyIORef timeRef (+0.25)
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
spawn :: Int -> Int -> Double -> Render ()
spawn x y t = do
    let (rx, ry) = centerCoords x y 
    cell rx ry
    redish t
    fill
kill :: Int -> Int -> Double -> Render ()
kill x y t = do
    let (rx, ry) = centerCoords x y 
    cell rx ry
    redish (1-t)
    fill
keepAlive :: Int -> Int -> Double -> Render ()
keepAlive x y t = do
    let (rx, ry) = centerCoords x y 
    cell rx ry
    setSourceRGBA 1 0 0 0.8
    fill
keepDead x y t = return ()




ft :: Double
ft = 1
transf :: GridDelta -> [Animation]
transf g = traverseFilter g tr kd 
  where tr ((x,y), Kill)      = mkDynamic ft (kill  x y) 
        tr ((x,y), Spawn)     = mkDynamic ft (spawn x y)
        tr ((x,y), KeepDead)  = mkDynamic ft (keepDead x y)
        tr ((x,y), KeepAlive) = mkDynamic ft (keepAlive x y)
        kd ((x,y), KeepDead) = False
        kd _ = True


--animationList ::  [Animation]
animationList gr = map (\g -> combAnim (transf g) ) (deltaGridList gr)
-- Slower if animationList depends on grid?

